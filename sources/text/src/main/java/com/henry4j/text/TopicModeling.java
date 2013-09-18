package com.henry4j.text;

import static com.google.common.io.ByteStreams.toByteArray;
import static java.lang.Math.max;
import static java.lang.String.format;
import static org.apache.hadoop.io.SequenceFile.Reader.length;
import static org.apache.hadoop.io.SequenceFile.Reader.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

import lombok.Cleanup;
import lombok.Getter;
import lombok.SneakyThrows;
import lombok.val;
import lombok.experimental.Accessors;
import lombok.extern.log4j.Log4j;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.fs.PositionedReadable;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.SequenceFile;
import org.apache.hadoop.io.Text;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.shingle.ShingleFilter;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;
import org.apache.lucene.search.similarities.DefaultSimilarity;
import org.apache.lucene.search.similarities.TFIDFSimilarity;
import org.apache.mahout.clustering.lda.cvb.TopicModel;
import org.apache.mahout.common.Pair;
import org.apache.mahout.common.lucene.AnalyzerUtils;
import org.apache.mahout.common.lucene.IteratorTokenStream;
import org.apache.mahout.math.DenseMatrix;
import org.apache.mahout.math.DenseVector;
import org.apache.mahout.math.RandomAccessSparseVector;
import org.apache.mahout.math.SequentialAccessSparseVector;
import org.apache.mahout.math.SparseRowMatrix;
import org.apache.mahout.math.Vector;
import org.apache.mahout.math.VectorWritable;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.henry4j.common.io.SeekableByteArrayInputStream;

@Log4j
@Accessors(fluent = true)
public class TopicModeling {
    final Configuration conf = new Configuration();
    final Analyzer analyzer;
    final TopicModel model;
    final Map<String, Integer> termToIdMappings;
    final double[] idf;
    @Getter final int maxNGram;
    final Field valuesField;

    @SneakyThrows({ ClassNotFoundException.class, NoSuchFieldException.class })
    public TopicModeling(InputStream dictionaryIS, InputStream modelIS, String analyzerName, InputStream dfIS) {
        String[] d = readDictionary(dictionaryIS, conf);
        maxNGram = d[d.length - 1].split(" ").length;
        analyzer = AnalyzerUtils.createAnalyzer(analyzerName);
        model = readModel(d, modelIS, conf);
        termToIdMappings = termToIdMappingsOf(d);
        idf = null == dfIS ? null : readIDFs(new DefaultSimilarity(), dfIS, conf);
        valuesField = DenseVector.class.getDeclaredField("values");
        valuesField.setAccessible(true);
    }

    public TopicModeling(InputStream dictionaryIS, InputStream modelIS, String analyzerName) {
        this(dictionaryIS, modelIS, analyzerName, null);
    }

    public int numTerms() {
        return model.getNumTerms();
    }

    public int numTopics() {
        return model.getNumTopics();
    }

    public int numDocs() {
        return null == idf ? -1 : idf.length;
    }

    @SneakyThrows({ IllegalArgumentException.class, IllegalAccessException.class })
    public double[] getPTopic(String text) { // also called p(z|d)
        val doc = vectorize(text);
        val docTopics = new DenseVector(new double[model.getNumTopics()]).assign(1.0 / model.getNumTopics());
        val docTopicModel = new SparseRowMatrix(model.getNumTopics(), doc.size());
        for (int i = 0; i < 20 /* maxItrs */; i++) {
            model.trainDocTopicModel(doc, docTopics, docTopicModel);
            log.debug(format("docTopics: %s.", shorten(docTopics, 0.02).toString()));
        }
        return (double[])valuesField.get(docTopics);
    }

    public Vector vectorize(String text) {
        val tf = tf(termToIdMappings, tokenize(text), maxNGram);
        return (null == idf) ? tf : tfidf(tf, idf);
    }

    @SneakyThrows({ IOException.class })
    public List<String> tokenize(String text) {
        @Cleanup TokenStream stream = analyzer.tokenStream("{field-name}", new StringReader(text));
        val termAttr = stream.addAttribute(CharTermAttribute.class);
        val tokens = ImmutableList.<String>builder();
        stream.reset();
        while (stream.incrementToken()) {
            if (termAttr.length() > 0) {
                val term = new String(termAttr.buffer(), 0, termAttr.length());
                tokens.add(term);
            }
        }
        return tokens.build();
    }

    static Vector tf(Map<String, Integer> termToIdMappings, String[] terms) {
        val doc = new RandomAccessSparseVector(termToIdMappings.size());
        for (val t : terms) {
            val id = termToIdMappings.get(t);
            if (null != id) {
                doc.setQuick(id, 1 + doc.getQuick(id));
            }
        }
        return doc;
    }

    @SneakyThrows({ IOException.class })
    static Vector tf(Map<String, Integer> termToIdMappings, List<String> terms, int maxNGram) {
        val doc = new RandomAccessSparseVector(termToIdMappings.size());
        if (maxNGram > 1) {
            @Cleanup TokenStream stream = new ShingleFilter(new IteratorTokenStream(terms.iterator()), maxNGram);
            val termAttr = stream.addAttribute(CharTermAttribute.class);
            stream.reset();
            while (stream.incrementToken()) {
                if (termAttr.length() > 0) {
                    val term = new String(termAttr.buffer(), 0, termAttr.length());
                    val id = termToIdMappings.get(term);
                    if (null != id) {
                        doc.setQuick(id, (doc.getQuick(id) + 1));
                    }
                }
            }
        } else {
            for (val term : terms) {
                val id = termToIdMappings.get(term);
                if (null != id) {
                    doc.setQuick(id, (doc.getQuick(id) + 1));
                }
            }
        }
        return doc;
    }

    static Vector tfidf(Vector tf, double[] idf) {
        val tfidf = new SequentialAccessSparseVector(tf.size());
        for (val e : tf.nonZeroes()) {
            tfidf.setQuick(e.index(), tf.getQuick(e.index()) * idf[e.index()]);
        }
        return tfidf;
    }

    static Vector shorten(Vector v, double min) {
        val shorten = new SequentialAccessSparseVector(v.size());
        for (val e : v.all()) {
            if (e.get() > min) {
                shorten.setQuick(e.index(), e.get());
            }
        }
        return shorten;
    }

    static Vector readVector(InputStream documentIS, Configuration conf, int offset) {
        return readVectorsInRange(documentIS, conf, offset, 1)[0].getSecond();
    }

    @SneakyThrows({ IOException.class })
    static Pair<String, Vector>[] readVectorsInRange(InputStream documentIS, Configuration conf, int offset, int length) {
        @Cleanup val reader = new SequenceFile.Reader(conf, asReaderOptions(documentIS));
        val documentName = new Text();
        @SuppressWarnings("unchecked")
        Pair<String, Vector>[] vectors = new Pair[length];
        VectorWritable vector = new VectorWritable();
        for (int i = 0; i < offset + length && reader.next(documentName, vector); i++) {
            if (i >= offset) {
                vectors[i - offset] = Pair.of(documentName.toString(), vector.get());
            }
        }
        return vectors;
    }

    static TopicModel readModel(String[] dictionary, InputStream modelIS, Configuration conf) {
        double alpha = 0.0001; // default: doc-topic smoothing
        double eta = 0.0001; // default: term-topic smoothing
        double modelWeight = 1f;
        val model = loadModel(conf, modelIS);
        return new TopicModel(model.getFirst(), model.getSecond(), eta, alpha, dictionary, 1, modelWeight);
    }

    @SneakyThrows({ IOException.class })
    static Pair<DenseMatrix, DenseVector> loadModel(Configuration conf, InputStream modelIS) {
        int numTopics = -1;
        int numTerms = -1;
        val rows = ImmutableList.<Pair<Integer, Vector>>builder();
        @Cleanup val reader = new SequenceFile.Reader(conf, asReaderOptions(modelIS));
        val iw = new IntWritable();
        val vw = new VectorWritable();
        while (reader.next(iw, vw)) {
            rows.add(Pair.of(iw.get(), vw.get()));
            numTopics = Math.max(numTopics, iw.get());
            if (numTerms < 0) {
                numTerms = vw.get().size();
            }
        }
        numTopics++;
        val topicTermCounts = new DenseMatrix(numTopics, numTerms);
        val topicSums = new DenseVector(numTopics);
        for (val pair : rows.build()) {
            topicTermCounts.viewRow(pair.getFirst()).assign(pair.getSecond());
            topicSums.set(pair.getFirst(), pair.getSecond().norm(1));
        }
        return Pair.of(topicTermCounts, topicSums);
    }

    @SneakyThrows({ IOException.class })
    static String[] readDictionary(InputStream is, Configuration conf) {
        val term = new Text();
        val id = new IntWritable();
        @Cleanup val reader = new SequenceFile.Reader(conf, asReaderOptions(is));
        val termIds = ImmutableList.<Pair<String, Integer>> builder();
        int maxId = -1;
        while (reader.next(term, id)) {
            termIds.add(Pair.of(term.toString(), id.get()));
            maxId = max(maxId, id.get());
        }
        val terms = new String[maxId + 1];
        for (val termId : termIds.build()) {
            terms[termId.getSecond().intValue()] = termId.getFirst().toString();
        }
        return terms;
    }

    static Map<String, Integer> termToIdMappingsOf(String[] terms) {
        val termToIdMappings = ImmutableMap.<String, Integer>builder();
        for (int i = 0; i < terms.length; i++) {
            termToIdMappings.put(terms[i], i);
        }
        return termToIdMappings.build();
    }

    @SneakyThrows({ IOException.class })
    static long[] readDFs(Path path, Configuration conf) {
        @Cleanup val is = FileSystem.get(conf).open(path);
        return readDFs(is, conf);
    }

    @SneakyThrows({ IOException.class })
    static long[] readDFs(InputStream is, Configuration conf) {
        val id = new IntWritable();
        val frequency = new LongWritable();
        @Cleanup val reader = new SequenceFile.Reader(conf, asReaderOptions(asFSDataIS(is)));
        val idFrequencies = ImmutableList.<Pair<Integer, Long>> builder();
        int maxId = -1;
        long numDocs = 0;
        while (reader.next(id, frequency)) {
            if (-1 == id.get()) {
                numDocs = frequency.get();
            } else {
                idFrequencies.add(Pair.of(id.get(), frequency.get()));
                maxId = max(maxId, id.get());
            }
        }
        val dfs = new long[maxId + 2];
        for (val idFrequency : idFrequencies.build()) {
            dfs[idFrequency.getFirst().intValue()] = idFrequency.getSecond().longValue();
        }
        dfs[maxId + 1] = numDocs;
        return dfs;
    }

    @SneakyThrows({ IOException.class })
    static double[] readIDFs(TFIDFSimilarity similarity, InputStream is, Configuration conf) {
        val id = new IntWritable();
        val frequency = new LongWritable();
        @Cleanup val reader = new SequenceFile.Reader(conf, asReaderOptions(asFSDataIS(is)));
        val idFrequencies = ImmutableList.<Pair<Integer, Long>> builder();
        int maxId = -1;
        long numDocs = 0;
        while (reader.next(id, frequency)) {
            if (-1 == id.get()) {
                numDocs = frequency.get();
            } else {
                idFrequencies.add(Pair.of(id.get(), frequency.get()));
                maxId = max(maxId, id.get());
            }
        }
        val idfs = new double[maxId + 1];
        for (val idFrequency : idFrequencies.build()) {
            idfs[idFrequency.getFirst().intValue()] = similarity.idf(idFrequency.getSecond().longValue(), numDocs);
        }
        return idfs;
    }

    @SneakyThrows({ IOException.class })
    private static SequenceFile.Reader.Option[] asReaderOptions(InputStream is) {
        val fdis = asFSDataIS(is);
        return new SequenceFile.Reader.Option[] { stream(fdis), length(fdis.available()) };
    }

    @SneakyThrows({ IOException.class })
    private static FSDataInputStream asFSDataIS(InputStream is) {
        if (is instanceof FSDataInputStream) {
            return (FSDataInputStream)is;
        } else {
            is = is instanceof PositionedReadable ? is : new SeekableByteArrayInputStream(toByteArray(is));
            return new FSDataInputStream(is);
        }
    }
}
