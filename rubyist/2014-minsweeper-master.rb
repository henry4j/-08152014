#!/usr/bin/env /usr/local/bin/jruby

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      r, c, m = io.readline.chomp.split.map { |e| e.to_i }
      [tc, r, c, m]
    end
    cases.each { |e| print solve(*e) }
  end

  def self.solve(tc, r, c, m)
    g = draw([r, c].min, [r, c].max, m)
    g = g.transpose if g && r > c
    s = g ? g.map { |e| e.join('') }.join("\n") : "Impossible"
    sprintf('Case #%d (%d x %d of %d):\n%s', tc, r, c, m, s)
  end

  def self.draw(r, c, m)
    n = r * c - m
    case
    when m == 0 || n == 1
      g = Array.new(r) { Array.new(c, n == 1 ? '*' : '.') }
      g[0][0] = 'c'
      g
    when r == 1 || n >= 4
      case
      when m % r == 0
        g = Array.new(r) { Array.new(c, '.') }
        m, n = m/r, n/r
        r.times { |j| (c-m).upto(c-1) { |i| g[j][i] = '*' } }
        g[0][0] = 'c' if n > 0
        g
      when r != 2 && n != 5 && n != 7
        g = Array.new(r) { Array.new(c, '*') }
        r.times { |j| (n/r).times { |i| g[j][i] = '.' } }
        (n%r).times { |j| g[j][n/r] = '.' }
        if n%r == 1
          g[r-1][n/r-1] = '*'
          g[1][n/r] = '.'
        end
        g[0][0] = 'c'
        g
      end
    end
  end
end

if ENV['DBGP_RUBY_PORT']
  require 'test/unit'

  class TestCases < Test::Unit::TestCase
    def test_main
      src = 'https://raw.github.com/henry4j/-/master/rubyist/minesweeper.in'
      dst = '/tmp/minesweeper.in'
      system 'curl -o %s -kL %s' % [dst, src] unless File.exists?(dst)
      open(dst) { |io| CodeJam.main(io) }
    end
  end
else
  CodeJam.main(ARGF)
end
