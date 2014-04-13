#!/usr/bin/env /usr/local/bin/ruby

# %w{test/unit open-uri}.each { |e| require e }

module CodeJam
  @@memos = [
    {},
    {},
    {2 => 1},
    {3 => 1},
    {2 => 2},
    {5 => 1},
    {2 => 1, 3 => 1},
    {7 => 1},
    {2 => 3}
  ]

  def self.main(io)
    io.readline
    r, n, m, k = io.readline.chomp.split.map { |e| e.to_i }
    p = 1.upto(r).map do |tc|
      io.readline.chomp.split.map { |e| e.to_i }
    end
    puts "Case #1:"
    p.each { |e| puts take_guess(n, m, k, e) }
  end

  def self.take_guess(n, m, k, p)
    max_factors = []
    p.each do |r|
      factors = []
      while r != 1
        [2, 3, 5, 7].each do |e|
          if 0 == r % e
            r /= e
            factors[e] = 1 + (factors[e] || 0)
            break
          end
        end
      end
      [2, 3, 5, 7].each do |e|
        max_factors[e] = [max_factors[e], factors[e]].compact.max
      end
    end
    v = max_factors.each_with_index.reduce(1) { |v, (e, i)| e ? v * i ** e : v }
    a = []
    n.times do
      unless v == 1 || v == 2
        m.downto(3) do |e|
          if 0 == v % e
            v /= e
            a << e
            break
          end
        end
      else
        a << 2
      end
    end
    a.join
  end
end

#class TestCases < Test::Unit::TestCase
#  def test_main
#    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/rubyist/good-luck-testcases/C-small-practice-1.in'
#    open(test_case_uri) { |io| CodeJam.main(io) }
#  end
#end

CodeJam.main(STDIN)
