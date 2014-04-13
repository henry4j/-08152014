#!/usr/bin/env /usr/local/bin/ruby

#%w{test/unit open-uri}.each { |e| require e }

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      lbound, ubound = io.readline.chomp.split.map { |e| e.to_i }
      [tc, lbound, ubound]
    end

    n = Math.sqrt(cases.map { |e| e[-1] }.max).ceil
    m = 8
    s = (n*1.0/m).ceil
    slices = m.times.map { |i| ((s * i) + 1)..(s * (i + 1)) }
    fair_n_squares = slices.map { |s| s.select { |e| fair_n_squares?(e) }.map { |e| e * e } }.reduce(:+)
    cases.each_slice((cases.size*1.0/m).ceil).each { |s| puts s.map { |c| count_fair_n_squares(fair_n_squares, *c) } }

#    fair_n_squares = slices.map { |s| Thread.new { s.select { |e| fair_n_squares?(e) }.map { |e| e * e } } }.map { |e| e.value}.reduce(:+)
#    cases.each_slice((cases.size*1.0/m).ceil).map { |s| Thread.new { s.map { |c| count_fair_n_squares(fair_n_squares, *c) } } }.each { |e| puts e.value }
  end

  def self.count_fair_n_squares(fair_n_squares, tc, lbound, ubound)
    lbound = fair_n_squares.index { |e| e >= lbound }
    ubound = fair_n_squares.index { |e| e > ubound } || fair_n_squares.size
    "Case ##{tc}: #{ubound - lbound}"
  end

  def self.fair_n_squares?(n)
    palindromic?(n) and palindromic?(n * n)
    # palindromic_s?(n.to_s) and palindromic_s?((n * n).to_s)
  end

  def self.palindromic_s?(s)
    ((n = s.size)/2).times.all? { |i| s[i] == s[n - i - 1] }
  end

  def self.palindromic?(n)
    n == reverse(n)
  end

  def self.reverse(n)
    rev = 0;
    rev, n = rev * 10 + n % 10, n / 10 while n > 0
    rev
  end
end

#class TestCases < Test::Unit::TestCase
#  def test_palindromic_s
#    assert CodeJam.palindromic_s?(121.to_s)
#    assert CodeJam.palindromic_s?(1221.to_s)
#  end
#
#  def test_main
#    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/rubyist/fair-n-square-testcases/C-large-practice-1.in'
#    open(test_case_uri) { |io| CodeJam.main(io) }
#  end
#end

CodeJam.main(STDIN)
