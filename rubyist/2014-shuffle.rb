#!/usr/bin/env /usr/local/bin/ruby

n = ARGV[0].to_i
exp = n.times.reduce([]) { |a, e| a[e] = e == 0 ? 1 : a[e-1] * 10; a }
c = n.times.reduce(0) { |s, e| s += e * exp[n-e-1]; s }

swap = lambda do |c, i, j|
  case
  when i == j
    c
  else
    i_exp, j_exp = exp[n-i-1], exp[n-j-1]
    i_dec, j_dec = c/i_exp%10, c/j_exp%10
    c += i_exp * (j_dec - i_dec)
    c += j_exp * (i_dec - j_dec)
  end
end

module Search
  def self.backtrack(candidate, expand_out, reduce_off)
    unless reduce_off.call(candidate)
      expand_out.call(candidate).each do |e|
        candidate.push e
        backtrack(candidate, expand_out, reduce_off)
        candidate.pop
      end
    end
  end
end

s = {}
expand_out = lambda do |a|
  i, c = a[-1]
  o = []
  n.times do |j|
    c = swap.call(c, i, j) unless i == j
    o << [i+1, c]
    c = swap.call(c, i, j) unless i == j
  end
  o
end

reduce_off = lambda do |a|
  i, c = a[-1]
  s[c] = (s[c] || 0) + 1 if i == n
end

Search.backtrack([[0, c]], expand_out, reduce_off)
puts s

#$ time ./2014-shuffle.rb 8
#real  0m33.189s
#user  0m32.793s
#sys 0m0.033s

#if ENV['DBGP_RUBY_PORT']
#  require 'test/unit'
#
#  class TestCases < Test::Unit::TestCase
#    def test_main
#      n = 100
#    end
#  end
#else
#  CodeJam.main(ARGV[0].to_i)
#end
