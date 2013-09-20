#!/usr/bin/env jruby
require 'rake'

def x!(*cmd, &blk) block_given? ? (sh cmd.join(' ') do |*a| blk.call(a) end) : (sh cmd.join(' ')) end

module Mahout
  def self.require_jars
    %w(
      com.google.guava:guava:14.0.1
      commons-logging:commons-logging:1.1.1
      log4j:log4j:1.2.17
      org.apache.avro:avro:1.5.3
      org.apache.commons:commons-math3:3.2
      #org.apache.hadoop:hadoop-common:2.0.5-alpha
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

  def self.write_vectors(path, vectors, conf = org.apache.hadoop.conf.Configuration.new)
    @@jars ||= require_jars
    path = org.apache.hadoop.fs.Path.new('file://' + File.absolute_path(path)) if path.is_a? String
    org.apache.mahout.math.MatrixUtils.write(path, conf, vectors)
  end

  def self.read_vectors(path, conf = org.apache.hadoop.conf.Configuration.new)
    path = org.apache.hadoop.fs.Path.new('file://' + File.absolute_path(path)) if path.is_a? String
    fs = org.apache.hadoop.fs.FileSystem.get(conf)
    reader = org.apache.hadoop.io.SequenceFile::Reader.new(fs, path, conf)
    key, value = reader.key_class.new_instance, reader.value_class.new_instance
    vectors = {}
    vectors[key.get] = value.get while reader.next(key, value) 
    vectors
  end
end

Mahout.require_jars
