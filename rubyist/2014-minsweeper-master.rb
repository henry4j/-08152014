#!/usr/bin/env /usr/local/bin/jruby

%w{test/unit open-uri}.each { |e| require e }
# %w{open-uri}.each { |e| require e }

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      r, c, m = io.readline.chomp.split.map { |e| e.to_i }
      [tc, r, c, m]
    end
    cases.each { |e| puts draw(*e) }
  end

  def self.draw(tc, r, c, m)
    s = case
    when r == 1
      if m > c - 2
        s = "Impossible\n"
      else
        s = "c"
        (r - m - 1).times { s += "." }
        m.times { s += "*" }
        s += "\n"
      end
    when c == 1
      if m > r - 2 
        s = "Impossible\n"
      else
        s = "c\n"
        (r - m - 1).times { s += ".\n" }
        m.times { s += "*\n" }
        s
      end
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
    "Case ##{tc}:\n" + s
  end
end

# CodeJam.main(STDIN)

class TestCases < Test::Unit::TestCase
  def test_main
    test_case_uri = 'https://raw.githubusercontent.com/henry4j/-/master/algorist/rubyist/minesweeper-testcases/small.in'
    open(test_case_uri) { |io| CodeJam.main(io) }
  end
end
