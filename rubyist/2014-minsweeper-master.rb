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
    g = g.transpose if r > c
    s = g.map { |e| e.join('') }.join("\n")
    "Case ##{tc}:\n#{s}"
  end

  def self.draw(r, c, m)
    g = Array.new(r) { Array.new(c) }
    n = r * c - m
    case
    when m == 0 || n == 1
      g = Array.new(r) { Array.new(c, n == 1 ? '*' : '.') }
      g[0][0] = 'c'
      g
    when r == 1 || n >= 4
      case
      when m % r == 0
        g = Array.new(r) { Array.new('.') }
        m, n = m/r, n/r
        r.times { |k| (c-m).upto(c-1) { |i| g[k][i] = '*' } }
        g[0][0] = 'c'
        g
      when r != 2
        
      end
  end
end

puts CodeJam.solve(0, 7, 2, 4)
#if ENV['DBGP_RUBY_PORT']
#  require 'test/unit'
#
#  class TestCases < Test::Unit::TestCase
#    def test_main
#      src = 'https://raw.github.com/henry4j/-/master/rubyist/minesweeper.in'
#      dst = '/tmp/minesweeper.in'
#      system 'curl -o %s -kL %s' % [dst, src] unless File.exists?(dst)
#      open(dst) { |io| CodeJam.main(io) }
#    end
#  end
#else
#  CodeJam.main(ARGF)
#end
