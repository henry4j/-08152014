#!/usr/bin/env ruby

# http://www.kelvinjiang.com/2010/10/facebook-puzzles-gattaca.html

%w{test/unit open-uri}.each { |e| require e }

class DP
  def self.optimal_schedule(events, memos = {})
    events = events.sort_by { |e| e[1] }
    possibles = (0...events.size).map { |i|
      (i-1).downto(0).find { |j| events[j][1] < events[i][0] }
    } # included =? excluded =? possible =?

    map = lambda do |n|
      if n == 0
        0
      else
        [
          events[n-1][2] + (possibles[n-1] ? map.call(possibles[n-1]+1) : 0),
          map.call(n-1)
        ].max
      end
    end
    map.call(events.size)
  end
end

class TestCases < Test::Unit::TestCase
  def test_gattaca
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/gattaca-testcases/input00.txt'
    points = []
    intervals = []
    open(test_case_uri) do |io|
      n = Integer(io.readline.chomp)
      n = (1 + (n-1)/80)
      n.times { io.readline }
      n = Integer(io.readline.chomp)
      n.times { intervals << io.readline.split.map { |e| Integer(e) } }
    end
    assert_equal 100, DP.optimal_schedule(intervals)
  end

  def test_bsearch
    a = [1, 1, 2, 3, 3, 3, 4, 4, 4, 4]
    assert_equal 5, a.bsearch_last_by { |e| 3 <=> e }
    assert_equal nil, a.bsearch_last_by { |e| 5 <=> e }
    assert_equal 9, a.bsearch_last_by { |e| 4 <=> e }
    assert_equal 2, a.bsearch_last_by { |e| 2 <=> e }
    assert_equal 1, a.bsearch_last_by { |e| 1 <=> e }
    assert_equal nil, a.bsearch_last_by { |e| 0 <=> e }
    assert_equal nil, a.bsearch_range_by { |e| 5 <=> e }
    assert_equal 6..9, a.bsearch_range_by { |e| 4 <=> e }
    assert_equal 3..5, a.bsearch_range_by { |e| 3 <=> e }
    assert_equal 2..2, a.bsearch_range_by { |e| 2 <=> e }
    assert_equal 0..1, a.bsearch_range_by { |e| 1 <=> e }
    assert_equal nil, a.bsearch_range_by { |e| 0 <=> e }
  end
end

=begin

Gattaca

You have a DNA string that you wish to analyze. Of particular interest is which intervals of the string represent individual genes. You have a number of "gene predictions", each of which assigns a score to an interval within the DNA string, and you want to find the subset of predictions such that the total score is maximized while avoiding overlaps. A gene prediction is a triple of the form (start, stop, score). start is the zero-based index of the first character in the DNA string contained in the gene. stop is the index of the last character contained in the gene. score is the score for the gene. 

Input Specification

Your program will be passed the name of an input file on the command line. The contents of that file are as follows. 

The first line of the input contains only n, the length of the DNA string you will be given. 

The next ceiling(n / 80) lines each contain string of length 80 (or n % 80 for the last line) containing only the characters 'A', 'C', 'G', and 'T'. Concatenate these lines to get the entire DNA strand. 

The next line contains only g, the number of gene predictions you will be given. 

The next g lines each contain a whitespace-delimited triple of integers of the form
<start> <stop> <score>
representing a single gene prediction. No gene predictions will exceed the bounds of the DNA string or be malformed (start is non-negative and no more than stop, stop never exceeds n - 1). 

Example Input:
100
GAACTATCGCCCGTGCGCATCGCCCGTCCGACCGGCCGTAAGTCTATCTCCCGAGCGGGCGCCCGATCTCAAGTGCACCT
CACGGCCTCACGACCGTGAG
8
43  70  27
3   18  24
65  99  45
20  39  26
45  74  26
10  28  20
78  97  23
0   9   22

Output Specification

Print to standard out the score of the best possible subset of the gene predictions you are given such that no single index in the DNA string is contained in more than one gene prediction, followed by a newline. The total score is simply the sum of the scores of the gene predictions included in your final result. 

When constructing your output, you may only consider genes exactly as they are described in the input. If you find the contents of a gene replicated elsewhere in the DNA string, you are not allowed to treat the second copy as a viable gene. Your solution must be fast and efficient to be considered correct by the robot. 

Example Output:
100

=end


class Array
  def bsearch_range_by(&block)
    if first = bsearch_first_by(&block)
      first..bsearch_last_by(first...self.size, &block)
    end
  end

  def bsearch_first_by(range = 0...self.size, &block)
    if range.count > 1
      mid = range.minmax.reduce(:+) / 2
      case block.call(self[mid])
      when -1 then bsearch_first_by(range.min...mid, &block)
      when 1 then bsearch_first_by(mid+1..range.max, &block)
      else bsearch_first_by(range.min..mid, &block)
      end
    else
      range.min if 0 == block.call(self[range.min])
    end
  end

  def bsearch_last_by(range = 0...self.size, &block)
    if range.count > 1
      mid = (1 + range.minmax.reduce(:+)) / 2
      case block.call(self[mid])
      when -1 then bsearch_last_by(range.min...mid, &block)
      when 1 then bsearch_last_by(mid+1..range.max, &block)
      else bsearch_last_by(mid..range.max, &block)
      end
    else
      range.min if 0 == block.call(self[range.min])
    end
  end

  def quicksort_k!(k = 0, left = 0, right = self.size-1, &block)
    quickfind_k!(k, left, right, true, &block)
    self
  end

  def quickfind_k!(k = 0, left = 0, right = self.size-1, sort = false, &block)
    # http://en.wikipedia.org/wiki/Selection_algorithm#Optimised_sorting_algorithms
    if right > left
      pivot = partition(left, right, block_given? ? block : proc { |a, b| a <=> b })
      quickfind_k!(k, left, pivot-1, sort, &block) if sort || pivot > k
      quickfind_k!(k, pivot+1, right, sort, &block) if pivot < k
    end
    self
  end

  def partition(left, right, comparer)
    pivot = left + rand(right - left + 1) # select pivot between left and right
    self[pivot], self[right] = self[right], self[pivot]
    pivot = left
    (left...right).each do |i|
      if comparer.call(self[i], self[right]) < 0
        self[pivot], self[i] = self[i], self[pivot]
        pivot += 1
      end
    end
    self[pivot], self[right] = self[right], self[pivot]
    pivot
  end
end

