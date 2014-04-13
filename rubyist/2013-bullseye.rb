#!/usr/bin/env /usr/local/bin/ruby

# %w{test/unit open-uri}.each { |e| require e }

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      r, t = io.readline.chomp.split.map { |e| e.to_i }
      [tc, r, t]
    end
    cases.each { |e| puts draw(*e) }
  end

  def self.draw(tc, r, t)
    t0 = 2 * r + 1
    n = 1
    n *= 2 until n * (t0 + 2 * n - 2) > t
    m = n / 2
    while m < n - 1
      p = (m + n) / 2
      if p * (t0 + 2 * p - 2) <= t
        m = p
      else
        n = p
      end
    end
    "Case ##{tc}: #{m}"
  end
end

class TestCases < Test::Unit::TestCase
  def test_main
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/rubyist/bullseye-testcases/A-small-attempt0.in'
    open(test_case_uri) { |io| CodeJam.main(io) }
  end
end

CodeJam.main(STDIN)
