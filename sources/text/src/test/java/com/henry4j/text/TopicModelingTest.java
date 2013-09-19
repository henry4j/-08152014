package com.henry4j.text;

import static java.lang.Math.round;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;

import lombok.val;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.charfilter.HTMLStripCharFilter;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;
import org.junit.Test;

import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import com.google.common.io.CharStreams;

// References:
// http://mail-archives.apache.org/mod_mbox/mahout-user/201205.mbox/%3CCAKA-QbDg4DR3RTbv8KoYnMOnLXkbbqaXji0+WtrL5UdSdsKbOA@mail.gmail.com%3E
// http://mail-archives.apache.org/mod_mbox/mahout-user/201205.mbox/%3CCACYXym-0zg3zPor-SmpWr=D210B_6-YeNyyNtddNWpiU_otDrA@mail.gmail.com%3E

public class TopicModelingTest {
/*
 * This program needs a dictionary (of terms), an LDA model (built w/ tfidf), and a doc-frequency as follows:
   curl -o '/tmp/#1' -ksL 'https://dl.dropboxusercontent.com/u/47820156/mahout/bigram/{df-count-0,dictionary.file-0,model-0,tfidf-vectors-0}'
 */

    TopicModeling topicModeling;
    TopicModeling topicModelingTF;

    public TopicModelingTest() {
        this.topicModeling = new TopicModeling(asFileIS("/dictionary-0"), asFileIS("/model-0"), "com.henry4j.text.CommTextAnalyzer", asFileIS("/df-count-0"));
        this.topicModelingTF = new TopicModeling(asFileIS("/dictionary-0"), asFileIS("/model-0"), "com.henry4j.text.CommTextAnalyzer");
    }

    @Test
    public void testDF() throws IOException {
        val conf = new Configuration();
        long[] df = TopicModeling.readDFs(new Path("/tmp/true-l-lda-df-count-0"), conf);
        assert null != df;
    }

    @Test
    public void testFindPTopic() {
        val d66227241 = "Seller contacted us with a concern about a refund that needs to issued; however the buyer no longer has the card that was used; advised seller to have the buyer contact CS to advise them on how to receive the refund on to a different card";
        val docTopics = topicModeling.getPTopic(d66227241);
        assertThat(round(100 * docTopics[17]), equalTo(40L)); // this doc belongs to topic 19 with 47%.
        assertThat(round(100 * docTopics[13]), equalTo(32L));
        assertThat(round(100 * docTopics[2]), equalTo(17L));
        assertThat(round(100 * docTopics[16]), equalTo(11L));
    }

    @Test
    public void testTokenization() throws IOException {
        val d90746711 = "Re: Your selling privileges have been reinstated\n<html><HEAD><LINK rel=stylesheet type=text/css href=/webmail/static/deg/css/wysiwyg-3933289048.css\" media=all> <META name=GENERATOR content=\"MSHTML 9.00.8112.16470\"></HEAD> <BODY><BR>I am still unable to access that account.<BR><BR>Mar 20; 2013 02:18:40 PM; seller-performance+C316OXWICMO107-T3K8YCZV0LFU9Q@amazon.com wrote:<BR> <BLOCKQUOTE style=\"BORDER-LEFT: rgb(102;153;204) 3px solid\">Hello from Amazon.com.<BR><BR>Thank you for writing regarding your Amazon.com selling account. We have reviewed this situation and have reactivated your account.<BR><BR>We apologize for any inconvenience this has caused. In our efforts to protect our community; we sometimes err on the side of caution. <BR><BR>We appreciate your interest and wish you the best of luck selling on Amazon.com.<BR><BR>Regards;<BR><BR>Seller Performance Team<BR>Amazon.com<BR>http://www.amazon.com<BR></BLOCKQUOTE></BODY></html>\"";
        val reader = new HTMLStripCharFilter(new StringReader(d90746711));
        assertThat(CharStreams.toString(reader), equalTo("Re: Your selling privileges have been reinstated\n\n\n\n \n\n \n\nI am still unable to access that account.\n\nMar 20; 2013 02:18:40 PM; seller-performance+C316OXWICMO107-T3K8YCZV0LFU9Q@amazon.com wrote:\n \nHello from Amazon.com.\n\nThank you for writing regarding your Amazon.com selling account. We have reviewed this situation and have reactivated your account.\n\nWe apologize for any inconvenience this has caused. In our efforts to protect our community; we sometimes err on the side of caution. \n\nWe appreciate your interest and wish you the best of luck selling on Amazon.com.\n\nRegards;\n\nSeller Performance Team\nAmazon.com\nhttp://www.amazon.com\n\n\n\n\""));

        @SuppressWarnings("resource")
        TokenStream stream = new CommTextAnalyzer().createComponents("{field-name}", new StringReader(d90746711)).getTokenStream();
        val termAttr = stream.addAttribute(CharTermAttribute.class);
        stream.reset();
        val tokens = ImmutableList.<String>builder();
        while (stream.incrementToken()) {
            if (termAttr.length() > 0) {
                val term = new String(termAttr.buffer(), 0, termAttr.length());
                tokens.add(term);
            }
        }
        assertThat(Joiner.on(", ").join(tokens.build()), equalTo("sell, privileg, reinstat, unabl, access, account, pm, seller, perform, c316oxwicmo107, t3k8yczv0lfu9q, wrote, write, sell, account, review, situat, reactiv, account, apolog, inconveni, caus, effort, protect, commun, err, side, caution, interest, luck, sell, seller, perform, team, http"));
    }

    private static InputStream asFileIS(String path) {
        return TopicModelingTest.class.getResourceAsStream(path);
    }
}