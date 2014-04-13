#!/usr/bin/env /usr/local/bin/jruby

%w{test/unit open-uri}.each { |e| require e }
# %w{open-uri}.each { |e| require e }

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      c, f, x = io.readline.chomp.split.map { |e| e.to_f }
      p c, f, x
      [tc, c, f, x]
    end
    cases.each { |e| puts solve(*e) }
  end

  def self.solve(tc, c, f, x)
    max = x/2 # 50
    map = lambda do |e, g|
      puts g/f
      a = x/g
      if e + a > max
        max
      elsif a < c/g
        e + a
      else
        b = map.call(e + c/g, g+f)
        [e + a, b].min
      end # memo?
    end
    s = map.call(0, 2.0)
    "Case ##{tc}: #{s}"
  end
end

# CodeJam.main(STDIN)

class TestCases < Test::Unit::TestCase
  def test_main
    
    CodeJam.solve(4, 30.50000, 3.14159, 1999.19990)
#    test_case_uri = '/workspace/gits/henry4j/algorist/rubyist/cookie-clicker-testcases/small.in'
#    open(test_case_uri) { |io| CodeJam.main(io) }
  end
end
