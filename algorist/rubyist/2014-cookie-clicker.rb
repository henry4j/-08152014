#!/usr/bin/env /usr/local/bin/jruby

%w{test/unit open-uri}.each { |e| require e }
# %w{open-uri}.each { |e| require e }

module CodeJam
#  def self.main(io)
#    cases = 1.upto(io.readline.to_i).map do |tc|
#      r, c, m = io.readline.chomp.split.map { |e| e.to_i }
#      [tc, r, c, m]
#    end
#    cases.each { |e| puts draw(*e) }
#  end

  def self.solve(tc, c, f, x)
    max = x/2 # 50
    map = lambda do |e, g|
      a = x/g
      if e + c/g > max
        max
      elsif a < c/g
        e + a
      else
        b = map.call(e + c/g, g+f)
        [e + a, b].min
      end
    end
    s = map.call(0, 2.0)
    puts "Case ##{tc}: #{s}\n"
  end
end

# CodeJam.main(STDIN)

class TestCases < Test::Unit::TestCase
  def test_main
    CodeJam.solve(2, 500.0, 4.0, 2000.0)
    # test_case_uri = 'https://raw.githubusercontent.com/henry4j/-/master/algorist/rubyist/minesweeper-testcases/small.in'
    # open(test_case_uri) { |io| CodeJam.solve(2, 30.0, 2.0, 100.0) }
  end
end
