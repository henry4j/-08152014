#!/usr/bin/env /usr/local/bin/jruby

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      r, c, m = io.readline.chomp.split.map { |e| e.to_i }
      [tc, r, c, m]
    end
    cases.each { |e| puts solve(*e) }
  end

  def self.solve(tc, r, c, m)
    g = mine_field([r, c].min, [r, c].max, m)
    g = g.transpose if g && r > c
    s = g ? g.map { |e| e.join('') }.join("\n") : "Impossible"
    sprintf("Case #%d (%d x %d of %d):\n%s", tc, r, c, m, s)
  end

  def self.mine_field(r, c, m)
    n = r * c - m
    if r == 1 || n >= 4 && n != 5 && n != 7
      g = Array.new(r) { Array.new(c, '*') }
      r.times { |j| (n/r).times { |i| g[j][i] = '.' } }
      (n%r).times { |j| g[j][n/r] = '.' }
      g[r-1][n/r-1], g[1][n/r] = '*', '.' if n%r == 1
      g[0][0] = 'c' if n > 0
      g
    end
  end
end

if ENV['DBGP_RUBY_PORT']
  require 'test/unit'

  class TestCases < Test::Unit::TestCase
    def test_main
      dst = 'minesweeper.in'
      src = 'https://raw.github.com/henry4j/-/master/rubyist/minesweeper.in'
      system 'curl -o %s -kL %s' % [dst, src] unless File.exists?(dst)
      open(dst) { |io| CodeJam.main(io) }
    end
  end
else
  CodeJam.main(ARGF)
end
