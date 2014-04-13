#!/usr/bin/env /usr/local/bin/jruby

%w{test/unit}.each { |e| require e }

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
    while c/g > 1e-6
      m = e + x/g if e + x/g < m
      e += c/g
      g += f
    end
    sprintf("Case #%d: %.7f", tc, m)
  end
end

class TestCases < Test::Unit::TestCase
  def test_main
    system 'curl -o /tmp/small.in -kL https://raw.githubusercontent.com/henry4j/-/master/algorist/rubyist/cookie-clicker-testcases/small.in' unless File.exists? '/tmp/small.in'
    test_case_uri = '/tmp/small.in'
    open(test_case_uri) { |io| CodeJam.main(io) }
  end
end
