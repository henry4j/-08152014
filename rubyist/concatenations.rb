#!/usr/bin/env ruby

%w{test/unit open-uri}.each { |e| require e }

module Search
  def self.rpns_of(n, s = '')
  end
end

class Puzzle
  def self.edit_distance_of_rpns(io)
  end
end

class TestCases < Test::Unit::TestCase
  def test_edit_to_rpn
  end
end

# Puzzle.edit_distance_of_rpns(STDIN)

=begin

Given a list of words, L, that are all the same length, and a string, S, find the starting position of the substring ofS that is a concatenation of each word in L exactly once and without any intervening characters.  This substring will occur exactly once in S.
 
Example:
L: "fooo", "barr", "wing", "ding", "wing"
S: "lingmindraboofooowingdingbarrwingmonkeypoundcake"
                 fooowingdingbarrwing
Answer: 13
 
L: "mon", "key"
S: "monkey
    monkey
Answer: 0
 
L: "a", "b", "c", "d", "e"
S: "abcdfecdba"
         ecdba
Answer: 5
 
The first line of input specifies L, it will contain between 1 and 100 space-separated words, each between 1 and 10 characters long.
 
The second line of input specifies S, the line will contain a single word up to 1 million characters long.
 
The characters in both L and S should be treated as case-sensitive.

=end
