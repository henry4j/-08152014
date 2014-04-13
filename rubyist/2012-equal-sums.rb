#!/usr/bin/env /usr/local/bin/ruby

%w{test/unit open-uri}.each { |e| require e }

module Search
  def self.backtrack(candidate, expand_out, reduce_off)
    unless reduce_off.call(candidate)
      (expand_out.call(candidate) || []).each do |e|
        candidate.push e
        backtrack(candidate, expand_out, reduce_off)
        candidate.pop
      end
    end
  end
end

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      n, *a = io.readline.chomp.split.map { |e| e.to_i }
      [tc, a]
    end
    cases.each do |c| 
      puts equal_sums(*c)
    end
  end

  def self.equal_sums(tc, a, n = a.size)
    h = {}
    (1..n).each do |k|
      a.combination(k).each do |c|
        if h[s = c.reduce(:+)]
          return "Case ##{tc}:\n#{c.join(' ')}\n#{h[s].join(' ')}"
        end
        h[s] = c
      end
    end
    "Case ##{tc}:\nIMPOSSIBLE"
  end
end

class TestCases < Test::Unit::TestCase
  def test_main
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/rubyist/equal-sums-testcases/C-large-practice.in'
    open(test_case_uri) { |io| CodeJam.main(io) }
  end
end

CodeJam.main(STDIN)
