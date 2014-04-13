#!/usr/bin/env /usr/local/bin/jruby

# %w{test/unit open-uri}.each { |e| require e }
%w{open-uri}.each { |e| require e }

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      io.readline
      n, k = 2.times.map { io.readline.chomp.split.map { |e| e.to_f }.sort }
      [tc, n, k]
    end
    cases.each { |e| puts solve(*e) }
  end

  def self.solve(tc, n, k)
    y = deceitful_war(n.dup, k.dup)
    z = war(n, k)
    "Case ##{tc}: #{y} #{z}"
  end

  def self.deceitful_war(n, k)
    w = 0
    until n.empty?
      if n[-1] < k[-1]
        a = n.shift
        b = k.pop
      else
        a = n.pop
        b = k.pop
      end
      w += 1 if a > b
    end
    w
  end

  def self.war(n, k)
    w = 0
    h = k.each_with_index.reduce({}) { |h, (e, i)| h[e] = i; h }
    until n.empty?
      a = n.shift
      b = k.bsearch { |x| x > a && x != -1 }
      b = k[k.index { |x| x != 0 }] unless b
      k[h[b]] = 0
      w += 1 if a > b
    end
    w
  end
end

if ENV['DBGP_RUBY_PORT']
  require 'test/unit'

  class TestCases < Test::Unit::TestCase
    def test_main
      uri = 'https://raw.githubusercontent.com/henry4j/-/master/algorist/rubyist/deceitful-war-testcases/small.in'
      dst = '/tmp/small.in'
      system 'curl -o %s -kL %s' % [dst, src] unless File.exists?(dst)
      open(dst) { |io| CodeJam.main(io) }
    end
  end
else
  CodeJam.main(ARGF)
end
