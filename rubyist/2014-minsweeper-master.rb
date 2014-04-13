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
    s = case
    when r == 1
      n = r * c - m
      (c-m).upto(c-1) { |i| g[0][i] = '*' }
      1.upto(n-1) { |i| g[0][i] = '.'  }
      g[0][0] = 'c'
      g
    else
      if m > r * c - 4
        s = "Impossible\n"
      else
        s = "c."
        z = r * c - m - 4
        r.times do |k|
          s += ".." if k == 1
          if c - 2 > z
            z.times { s += "." }
            z = 0
            (c - 2 - z).times { s += "*" }
          else
            (c - 2).times { s += "." }
            z -= c - 2
          end
          s += "\n"
        end
        s
      end
    end
  end
end

puts CodeJam.solve(0, 2, 1, 1)
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
