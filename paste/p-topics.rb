#!/usr/bin/env jruby
%W(optparse json #{ENV['HADOOP_BASE']}/libexec/lib/text-1.0-SNAPSHOT.jar).each { |e| require e }

class TopicModeling
  def self.require_jars
    %w(
      com.google.guava:guava:14.0.1
      commons-logging:commons-logging:1.1.1
      log4j:log4j:1.2.17
      org.apache.avro:avro:1.5.3
      org.apache.commons:commons-math3:3.2
      org.apache.hadoop:hadoop-common:2.0.5-alpha
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

  def initialize(model_id, debug = false)
    @@jars ||= TopicModeling.require_jars
    a, b = "http://s3.amazonaws.com/#{ENV['S3_BUCKET']}/#{model_id}/{dictionary.file-0,model-0,df-count-0}", "/tmp/#{model_id}"
    system "curl -o '#{b}/#1' -ksL '#{a}' --create-dirs" unless Dir.exist?(b)
    $stderr.puts "DEBUG: openning model files (id: #{model_id})" if debug
    org.apache.log4j.Logger.root_logger.level = org.apache.log4j.Level::OFF
    dictionary, model, df = %w(dictionary.file-0 model-0 df-count-0).map { |e| open("#{b}/#{e}").to_inputstream }
    begin
      @modeling = com.henry4j.text.TopicModeling.new(dictionary, model, 'com.henry4j.text.CommTextAnalyzer', df)
    ensure
      [dictionary, model, df].each { |e| e and e.close }
    end
  end

  def p_topics(s)
    @modeling.get_ptopic(s).to_ary
  end
end

def parse_options
  options = {}
  OptionParser.new do |p|
    p.on('-d', '--debug',                    'Prints DEBUG messages') { |v| options[:debug] = v }
    p.on('-m', '--model_id STRING', String,  'Specifies optional model id.') { |v| options[:model_id] = v }
    p.on('-p', '--p-threshold FLOAT', Float, 'Specifies prediction threshold (default threshold: 7.1).') { |v| options[:p_threshold] = v }
  end.parse!
  options
end

def pp_zd(p_zd, threshold)
  z = case
  when p_zd[0].nan? then -1
  else
    zi = p_zd.each_with_index.max
    zi[0] > threshold ? zi[1] : -1
  end
  puts [z, p_zd.to_s.inspect].join(',')
end

def run!
  options = parse_options
  debug = options[:debug]
  model_id = options[:model_id] || 'unigram-rrc-pro-22k'
  threshold = options[:p_threshold] || 0.071

  modeling = TopicModeling.new(model_id, debug)
  if ARGV.empty?
    $stderr.puts 'DEBUG: begins reading lines' if debug
    pp_zd(modeling.p_topics($_), threshold) unless $_.chomp!.empty? while gets
    $stderr.puts 'DEBUG: ends reading lines' if debug
  else
    ARGV.each { |e| pp_zd(modeling.p_topics(e), threshold) }
  end
  exit 0
end

run! if __FILE__==$0
