#!/usr/bin/env ruby

%w{test/unit open-uri}.each { |e| require e }

module Search
  def self.rpns_of(n, s = '')
    if n == s.size
      [s.dup]
    else
      x = s.count('x')
      y = s.size - x
      c = case
      when x > n / 2 then ['*']
      when x > y + 1 then ['x', '*']
      else ['x']
      end
      c.reduce([]) { |a, e| a += rpns_of(n, s+e) }
    end
  end

  def self.combine_rpns(n)
    answers = []
    expand_out = proc do |a|
      x = a.count('x')
      s = a.size - x
      if x > n / 2
        ['*']
      elsif x > s + 1
        ['x', '*']
      else
        ['x']
      end
    end

    reduce_off = lambda do |a|
      answers << a.dup.join if a.size == n
    end

    Search.backtrack([], expand_out, reduce_off)
    answers
  end

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

class DP
  def self.edit_distance(s, t, bound = nil, whole = true, i = s.size, j = t.size, memos = [])
    memos[i] ||= []
    memos[i][j] ||= case
    when 0 == i then whole ? j : 0
    when 0 == j then i
    when bound && (j - i).abs >= bound then bound
    else
      [
        edit_distance(s, t, bound, whole, i-1, j-1, memos) + (s[i-1] == t[j-1] ? 0 : 1),
        edit_distance(s, t, bound, whole, i-1, j, memos) + 1,
        edit_distance(s, t, bound, whole, i, j-1, memos) + 1
      ].min
    end
  end
end

class Puzzle
  def self.edit_distance_of_rpns(io)
    n = io.readline.to_i
    n.times do
      s = io.readline.chomp
      rpns = Search.rpns_of(s.size).reduce({}) { |h, e| h.merge(e => nil) }
      if rpns.has_key?(s)
        puts 0
      else
        min = s.size
        rpns.keys.each do |t|
          min = [min, DP.edit_distance(s, t, min)].min
        end
        puts min
      end
    end
  end
end

class TestCases < Test::Unit::TestCase
  def test_edit_to_rpn
    assert_equal ["x"], Search.combine_rpns(1)
    assert_equal ["xx*"], Search.combine_rpns(3)
    assert_equal ["xxx**", "xx*x*"], Search.combine_rpns(5)
    assert_equal ["xxxx***", "xxx*x**", "xxx**x*", "xx*xx**", "xx*x*x*"], Search.combine_rpns(7)
    assert_equal ["xxxx***", "xxx*x**", "xxx**x*", "xx*xx**", "xx*x*x*"], Search.rpns_of(7)

    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/rpn-testcases/input00.txt'
    open(test_case_uri) do |io|
      Puzzle.edit_distance_of_rpns(io)
    end
  end
end

# Puzzle.edit_distance_of_rpns(STDIN)

=begin

An expression consisting of operands and binary operators can be written in Reverse Polish Notation (RPN) by writing both the operands followed by the operator. For example, 3 + (4 * 5) can be written as "3 4 5 * +".
 
You are given a string consisting of x's and *'s. x represents an operand and * represents a binary operator. It is easy to see that not all such strings represent valid RPN expressions. For example, the "x*x" is not a valid RPN expression, while "xx*" and "xxx**" are valid expressions. What is the minimum number of insert, delete and replace operations needed to convert the given string into a valid RPN expression?
 
Input:
The first line contains the number of test cases T. T test cases follow. Each case contains a string consisting only of characters x and *.
 
Output:
Output T lines, one for each test case containing the least number of operations needed.
 
Constraints:
1 <= T <= 100
The length of the input string will be at most 100.
 
Sample Input:
5
x
xx*
xxx**
*xx
xx*xx**
 
Sample Output:
0
0
0
2
0
 
Explanation:
For the first three cases, the input expression is already a valid RPN, so the answer is 0.
For the fourth case, we can perform one delete, and one insert operation: *xx -> xx -> xx*

=end
