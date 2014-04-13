#!/usr/bin/env /usr/local/bin/jruby

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      c, f, x = io.readline.chomp.split.map { |e| e.to_f }
      [tc, c, f, x]
    end
    cases.each { |e| puts solve(*e) }
  end

  def self.solve(tc, c, f, x)
    e = 0.0
    g = 2.0
    m = x/g
    while c/g > 1e-6 && g < x
      m = e + x/g if e + x/g < m
      e += c/g
      g += f
    end
    sprintf("Case #%d: %.7f", tc, m)
  end
end

if ENV['DBGP_RUBY_PORT']
  require 'test/unit'

  class TestCases < Test::Unit::TestCase
    def test_main
      src = 'https://raw.github.com/henry4j/-/master/algorist/rubyist/cookie-clicker-testcases/small.in'
      dst = '/tmp/small.in'
      system 'curl -o %s -kL %s' % [dst, src] unless File.exists?(dst)
      open(dst) { |io| CodeJam.main(io) }
    end
  end
else
  CodeJam.main(ARGF)
end
