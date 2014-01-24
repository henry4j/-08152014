#!/usr/bin/env jruby -E windows-1250

class SGD
  # http://mahout.apache.org/users/classification/logistic-regression.html
  def self.require_jars
    %w(
      com.google.guava:guava:16.0
      log4j:log4j:1.2.17
      org.apache.commons:commons-math3:3.2
      org.apache.hadoop:hadoop-core:1.2.1
      org.apache.lucene:lucene-analyzers-common:4.3.0
      org.apache.lucene:lucene-core:4.3.0
      org.apache.mahout:mahout-core:0.8
      org.apache.mahout:mahout-math:0.8
      org.slf4j:slf4j-api:1.6.1
      org.slf4j:slf4j-log4j12:1.6.1
    ).map do |e|
      g, a, v = e.split(':')
      jar = "#{ENV['HOME']}/.m2/repository/#{g.gsub(/\./, '/')}/#{a}/#{v}/#{a}-#{v}.jar"
      system "mvn dependency:get -DremoteRepositories=http://download.java.net/maven2 -Dartifact=#{e}" unless File.exist?(jar)
      require jar
    end
  end

  def initialize(categories, features)
    @@jars ||= SGD.require_jars
    @@values ||= org.apache.mahout.math.DenseVector.java_class.declared_field('values').tap { |f| f.accessible = true }
    @categories, @features = categories, features
  end

  def vectorize(text, ngram = 2, v = org.apache.mahout.math.RandomAccessSparseVector.new(@features))
    @enc ||= org.apache.mahout.vectorizer.encoders.AdaptiveWordValueEncoder.java_class.constructor(java.lang.String).new_instance('contents').to_java.tap { |e| e.probes = 2 }
    terms = text.split
    terms = ngram(terms, ngram) if ngram > 1
    terms.each { |w| @enc.addToVector(w, v) }
    v
  end

  def ngram(terms, max_ngram)
    # http://lucene.apache.org/core/4_6_0/analyzers-common/org/apache/lucene/analysis/shingle/ShingleFilter.html
    # token_stream = org.apache.mahout.common.lucene.IteratorTokenStream.new(java.util.Arrays.asList(terms.to_java(:string)).iterator)
    token_stream = org.apache.mahout.common.lucene.IteratorTokenStream.new(java.util.Arrays.asList(terms.to_java).iterator)
    stream  = org.apache.lucene.analysis.shingle.ShingleFilter.new(token_stream, max_ngram).tap { |e| e.reset }
    term_attr = stream.add_attribute(org.apache.lucene.analysis.tokenattributes.CharTermAttribute.java_class)
    tokens = []
    begin
      tokens << java.lang.String.new(term_attr.buffer, 0, term_attr.length).to_s if term_attr.length > 0 while stream.increment_token
      tokens
    ensure
      stream.close
    end
  end

  def train(category, instance)
    @lr ||= org.apache.mahout.classifier.sgd.AdaptiveLogisticRegression.new(@categories, @features, org.apache.mahout.classifier.sgd.L1.new)
    @lr.train(category, instance) # e in [actual, instance]
    self
  end

  def classify(instance)
    @lr.classifyFull(v = org.apache.mahout.math.DenseVector.new(@categories), instance)
    @@values.value(v).to_a
  end

  def close
    @lr.close
  end

  def write(model)
    dos = java.io.DataOutputStream.new(open(model, 'w').to_outputstream)
    begin
      org.apache.mahout.classifier.sgd.PolymorphicWritable.write(dos, @lr.best.payload.learner)
    ensure
      dos.close
    end
  end

  def read(model)
    dis = java.io.DataInputStream.new(open(model, 'r').to_inputstream)
    begin
      @lr = org.apache.mahout.classifier.sgd.PolymorphicWritable.read(dis, org.apache.mahout.classifier.sgd.CrossFoldLearner.java_class)
    ensure
      dis.close
    end
  end
end # the end of class SGD
