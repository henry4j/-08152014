#!/usr/bin/env jruby
require 'rake'

def x!(*cmd, &blk) block_given? ? (sh cmd.join(' ') do |*a| blk.call(a) end) : (sh cmd.join(' ')) end

module Vectors
  def self.conf
    @@hdfs_conf ||= org.apache.hadoop.conf.Configuration.new.tap { |e| e.set('fs.default.name', 'hdfs://localhost:9000/') }
  end

  def self.require_jars
    %w(
      com.google.guava:guava:14.0.1
      commons-configuration:commons-configuration:1.6
      commons-logging:commons-logging:1.1.1
      commons-lang:commons-lang:2.5
      log4j:log4j:1.2.17
      org.apache.avro:avro:1.5.3
      org.apache.commons:commons-math3:3.2
      org.apache.hadoop:hadoop-core:1.2.1
      org.apache.lucene:lucene-analyzers-common:4.3.0
      org.apache.lucene:lucene-core:4.3.0
      org.apache.mahout:mahout-core:0.8
      org.apache.mahout:mahout-math:0.8
      org.slf4j:slf4j-api:1.6.1
      org.slf4j:slf4j-log4j12:1.6.1
    ).map do |e|
      next false if e[0] == '#'
      g, a, v = e.split(':')
      jar = "#{ENV['HOME']}/.m2/repository/#{g.gsub(/\./, '/')}/#{a}/#{v}/#{a}-#{v}.jar"
      x! "mvn dependency:get -DremoteRepositories=http://download.java.net/maven2 -Dartifact=#{e}" unless File.exist?(jar)
      require jar
    end
  end

  def self.write(vectors, path, conf = Vectors.conf)
    fail "vectors must be 'VectorIterable'." unless vectors.is_a? org.apache.mahout.math.VectorIterable
    path = org.apache.hadoop.fs.Path.new(File.absolute_path(path)) if path.is_a?(String)
    fs = org.apache.hadoop.fs.FileSystem.get(conf)
    fs.delete(path)
    writer = org.apache.hadoop.io.SequenceFile::Writer.new(fs, conf, path, org.apache.hadoop.io.IntWritable.java_class, org.apache.mahout.math.VectorWritable.java_class)
    key, value = writer.key_class.new_instance, writer.value_class.new_instance
    begin
      vectors.each { |e| writer.append(key.tap { |k| k.set(e.index) }, value.tap { |v| v.set(e.vector) }) } 
    ensure
      writer.close
    end
  end

  def self.read(path, conf = Vectors.conf)
    path = org.apache.hadoop.fs.Path.new(File.absolute_path(path)) if path.is_a?(String)
    fs = org.apache.hadoop.fs.FileSystem.get(conf)
    reader = org.apache.hadoop.io.SequenceFile::Reader.new(fs, path, conf)
    key, value = reader.key_class.new_instance, reader.value_class.new_instance
    vectors = {}
    begin
      vectors[key.get] = value.get while reader.next(key, value) 
    ensure
      reader.close
    end
    vectors
  end

  def self.to_a(vector)
    vector.all.reduce([]) { |a, e| a[e.index] = e.get; a }
  end

  def self.to_h(vector)
    vector.non_zeroes.reduce({}) { |h, e| h[e.index] = e.get; h }
  end
end

Vectors.require_jars

def run!
  path = ARGV[0] || '/tmp/matrix.seq'
  vectors = org.apache.mahout.math.SparseRowMatrix.new(3, 4, true)
  vectors.view_row(0).set_quick(2, 4)
  vectors.view_row(1).set_quick(3, 5)
  Vectors.write(vectors, path)
  p Vectors.read(path).map { |(k, v)| Vectors.to_a(v) }
end

run! if __FILE__==$0
