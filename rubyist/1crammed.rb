#!/usr/bin/env ruby
# encoding: utf-8
# Ruby Styles https://github.com/bbatsov/ruby-style-guide

# * Arrays.transpose, quickfind_first, find_occurences, min_out_of_cycle, index_out_of_cycle
# * Strings.min_window_span, index_of_by_rabin_karp, and combine_parens
# * SNode.find_cycle; Numbers.divide, sum, minmax, and abs.
# * Miller-Rabin's primality test, 1 = a**(n-1) % n, when a is in (2..n-2).
# * Data Structures: Trie, BinaryHeap, LRUCache, CircularBuffer, and MedianBag
# * Graph: has_cycle? topological_sort that pushes v onto a stack on vertex exit.
# * Binary Tree: path_of_sum, common_ancestors, diameter, successor, and last(k).
# * integer partition & composition, and set partition by restricted growth string.
# * DP.optimal_tour (TSP), optimal_BST, edit distance, partition_bookshelf, subset sum.

%w{test/unit stringio set}.each { |e| require e }

module Partitions
  def self.int_partition(n = 0) # http://en.wikipedia.org/wiki/Partition_(number_theory)
    # e.g., the seven distinct integer partitions of 5 are 5, 4+1, 3+2, 3+1+1, 2+2+1, 2+1+1+1, and 1+1+1+1+1.
    case
    when 0 == n then []
    when 1 == n then [[1]]
    else
      int_partition(n-1).reduce([]) do |a,p|
        a << p[0..-2] + [p[-1]+1] if p[-2].nil? || p[-2] > p[-1]
        a << p + [1] # evaluates to self.
      end
    end
  end

  def self.int_composition(n, k = 1..n) # http://en.wikipedia.org/wiki/Composition_(number_theory)
    case
    when 0 == k then []
    when 1 == k then [[n]]
    when k.respond_to?(:reduce)
      k.reduce([]) { |a, e| a += int_composition(n, e) }
    else
      (1...n).reduce([]) { |a, i| a += int_composition(n-i, k-1).map { |c| [i] + c } }
    end
  end

  def self.set_partition(ary = [])
    # http://oeis.org/wiki/User:Peter_Luschny/SetPartitions
    prefix_maximums = Array.new(ary.size, 0)
    restricted_keys = Array.new(ary.size, 0)
    partitions = []
    while succ!(restricted_keys, prefix_maximums)
      partitions << ary.each_index.reduce([]) { |p, i|
        (p[restricted_keys[i]] ||= []) << ary[i]; p
      }
    end
    partitions
  end

  def self.succ!(restricted_keys, prefix_maximums)
    k = (restricted_keys.size - 1).downto(0) do |k|
      break k if 0 == k || restricted_keys[k] < prefix_maximums[k - 1] + 1
    end

    if k > 0 # else nil
      restricted_keys[k] += 1
      prefix_maximums[k] = [prefix_maximums[k], restricted_keys[k]].max
      (k + 1).upto(restricted_keys.size - 1) do |i|
        restricted_keys[i] = 0
        prefix_maximums[i] = prefix_maximums[i - 1]
      end
      restricted_keys
    end
  end
end

module Math
  def self.negate(a)
    neg = 0
    e = a < 0 ? 1 : -1
    while a != 0
      a += e
      neg += e
    end
    neg
  end

  def self.subtract(a, b)
    a + negate(b)
  end

  def self.abs(a)
    a < 0 ? negate(a) : a
  end

  def self.multiply(a, b)
    if abs(a) < abs(b)
      multiply(b, a)
    else
      v = abs(b).times.reduce(0) { |v, _| v += a }
      b < 0 ? negate(v) : v
    end
  end

  def self.different_signs(a, b)
    a < 0 && b > 0 || a > 0 && b < 0 ? true : false
  end

  def self.divide(a, b)
    if a < 0 && b < 0
      divide(negate(a), negate(b))
    elsif a < 0
      negate(divide(negate(a), b))
    elsif b < 0
      negate(divide(a, negate(b)))
    else
      quotient = 0
      divisor = negate(b)
      until a < b
        a += divisor
        quotient += 1
      end
      quotient
    end
  end

  def self.integer_of_prime_factors(k, factors = [3, 5, 7])
    queues = factors.map { |e| [e] }
    x = nil
    k.times do
      j = queues.each_index.reduce(0) { |j, i| queues[j][0] < queues[i][0] ? j : i }
      x = queues[j].shift
      (j...queues.size).each { |i| queues[i] << x * factors[i] }
    end
    x
  end
end

class Integer
  def self.gcd_e(a, b) # http://en.wikipedia.org/wiki/Euclidean_algorithm#Implementations
    if b == 0
      a
    else
      gcd_e(b, a % b)
    end
  end

  def factorize(m = 2) # http://benanne.net/code/?p=9
    @factors ||= case
    when 1 == self then []
    when 0 == self % m then [m] + (self / m).factorize(m)
    when Math.sqrt(self) <= m then [self]
    else factorize(m + (m == 2 ? 1 : 2))
    end
  end

  def factorize2
    n, m = self, 2
    factors = []
    loop do
      if 1 == n
        break factors
      elsif n % m == 0
        factors << m
        n /= m
      elsif Math.sqrt(n) <= m
        break factors << n
      else
        m += (m == 2 ? 1 : 2)
      end
    end
  end
end

###########################################################
# Test Cases of algorithm design manual & exercises
###########################################################

class TestCases < Test::Unit::TestCase
  def test_largest_rectangle_in_histogram
    h = [0, 3, 2, 1, 4, 7, 9, 6, 5, 4, 3, 2] # heights
    max_area = Arrays.max_area_in_histogram(h)
    assert_equal 24, max_area
  end

  def test_multiply_two_strings
    # https://gist.github.com/pdu/4978107
  end

  def test_sum_two_strings
    a = '12345'.reverse
    b = '123456789'.reverse
    c = ''
    zero = '0'.each_byte.next
    tens = ones = 0
    [a.size, b.size].max.times do |i|
      ones = tens
      ones += a[i,1].each_byte.next - zero if i < a.size
      ones += b[i,1].each_byte.next - zero if i < b.size
      tens, ones = ones / 10, ones % 10
      c += (ones + zero).chr
    end
    c += (tens + zero).chr if tens > 0
    c = c.reverse
    assert_equal '123469134', c
  end

  def test_rain_water
    a = [0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1]
    prefix_m = a.reduce([]) { |p, e| p.push(p.empty? ? e : [p.last, e].max) }
    suffix_m = a.reverse_each.reduce([]) { |p, e| p.push(p.empty? ? e : [p.last, e].max) }.reverse
    maxima = a.each_index.map { |i| [prefix_m[i], suffix_m[i]].min }
    volume = a.each_index.map { |i| maxima[i] - a[i] }
    assert_equal 6, volume.reduce(:+)
  end

  def test_longest_ranges
    assert_equal [1, 4], Arrays.longest_ranges([100, 4, 200, 1, 3, 2])
    assert_equal [1, 3, 5, 7], Arrays.longest_ranges([6, 7, 3, 9, 5, 1, 2])
    assert_equal [[-1, -1, 2], [-1, 0, 1]], Arrays.three_sum([-1, 0, 1, 2, -1, -4])
    assert_equal [[-1, 1, 2]], Arrays.three_sum_closest([-1, 2, 1, -4])
  end

  def test_solve_boggle
    m = [
      ['D', 'G', 'H', 'I'],
      ['K', 'L', 'P', 'S'],
      ['Y', 'E', 'U', 'T'],
      ['E', 'O', 'R', 'N']
    ]
    d = ['SUPER', 'LOB', 'TUX', 'SEA', 'FAME', 'HI', 'YOU', 'YOUR', 'I']
    d = d.reduce({}) { |h, e| h[e] = 0; h } # dictionary
    assert_equal ['HI', 'I', 'SUPER', 'YOU', 'YOUR'], Search.solve_boggle(d, m, 5) # max length (5)
  end

  def test_10_x_gcd_n_lcm
    assert_equal [2, 2, 3], 12.factorize
    assert_equal [2, 2, 3, 3], 36.factorize
    assert_equal [2, 2, 3, 3, 3], 108.factorize
    assert_equal [2, 2, 2, 3, 3], 72.factorize2
    assert_equal [2, 2, 2, 3], 24.factorize2
    assert_equal [101], 101.factorize2
    assert_equal 2**2 * 3**2, Integer.gcd_e(108, 72)
  end

  def test_traveling_salesman_problem
    # Problem: Robot Tour Optimization
    # Input: A set S of n points in the plane.
    # Output: What is the shortest cycle tour that visits each point in the set S?
    graph = []
    graph[0] = [0, 10, 15, 20]
    graph[1] = [5, 0, 9, 10]
    graph[2] = [6, 13, 0, 12]
    graph[3] = [8, 8, 9, 0]
    assert_equal [35, [0, 1, 3, 2, 0]], DP.optimal_tour(graph)
  end

  def test_subset_of_sum
    # http://www.youtube.com/watch?v=WRT8kmFOQTw&feature=plcp
    # suppose we are given N distinct positive integers
    # find subsets of these integers that sum up to m.
    assert_equal [[2, 5], [3, 4], [1, 2, 4]], DP.subset_of_sum([1, 2, 3, 4, 5], 7)
    assert_equal [[1, 2, 3]], DP.ordinal_of_sum(6, 3)
    assert_equal [[1, 5], [2, 4]], DP.ordinal_of_sum(6, 2)
  end

  def test_make_equation
    # Given N numbers, 1 _ 2 _ 3 _ 4 _ 5 = 10,
    # Find how many ways to fill blanks with + or - to make valid equation.
  end

  def test_bookshelf_partition
    assert_equal [[1, 2, 3, 4, 5], [6, 7], [8, 9]], DP.partition_bookshelf([1, 2, 3, 4, 5, 6, 7, 8, 9], 3)
  end

  def test_optimal_binary_search_tree
    keys = [5, 9, 12, 16, 25, 30, 45]
    probs = [0.22, 0.18, 0.20, 0.05, 0.25, 0.02, 0.08]
    assert_equal 1000, (1000 * probs.reduce(:+)).to_i
    assert_equal 2150, (1000 * DP.optimal_binary_search_tree(keys, probs)).to_i
  end

  def test_maze
    maze = []
    maze[0] = [1, 1, 1, 1, 1, 0]
    maze[1] = [1, 0, 1, 0, 1, 1]
    maze[2] = [1, 1, 1, 0, 0, 1]
    maze[3] = [1, 0, 0, 1, 1, 1]
    maze[4] = [1, 1, 0, 1, 0, 0]
    maze[5] = [1, 1, 0, 1, 1, 1]

    answers = []
    entered = [] # maze.size * r + c, e.g. 6*r + c
    expand_out = lambda do |a|
      r, c = a[-1]
      (entered[r] ||= [])[c] = true
      [[r-1, c], [r+1, c], [r, c-1], [r, c+1]].select { |p|
        p[0] > -1 && p[0] < maze.size && p[1] > -1 && p[1] < maze[0].size
      }.select { |p| !(entered[p[0]] ||= [])[p[1]] && 1 == maze[p[0]][p[1]] }
    end

    reduce_off = lambda do |a|
      answers << a.dup if a[-1][0] == maze.size-1 && a[-1][1] == maze[0].size-1
    end

    Search.backtrack([[0, 0]], expand_out, reduce_off) # a, i, input, branch, reduce
    assert_equal 1, answers.size
    assert_equal [5, 5], answers.last.last
  end

  def test_minmax_by_divide_n_conquer
    assert_equal 6, Arrays.peak([1, 5, 7, 9, 15, 18, 21, 19, 14])
    assert_equal [1, 9], Arrays.minmax([1, 3, 5, 7, 9, 2, 4, 6, 8])
    assert_equal [0, [0, 0]], Arrays.max_profit([30])
    assert_equal [30, [2, 3]], Arrays.max_profit([30, 40, 20, 50, 10])
  end

  def test_prime?
    assert Numbers.prime?(2)
    assert Numbers.prime?(3)
    assert Numbers.prime?(101)
    assert_equal 11, Numbers.prime(9)
  end

  def test_saurab_peaceful_queens
    assert_equal [[1, 3, 0, 2], [2, 0, 3, 1]], Search.queens_in_peace(4)
  end

  def test_order_matrix_chain_multiplication
    assert_equal [4500, [0, 1]], DP.order_matrix_chain_multiplication([10, 30, 5, 60])
    assert_equal [3500, [1, 0]], DP.order_matrix_chain_multiplication([50, 10, 20, 5])
  end

  def test_longest_common_n_increasing_subsequences
    assert_equal ["eca"], DP.longest_common_subsequence('democrat', 'republican')
    assert_equal ["1", "a"], DP.longest_common_subsequence('a1', '1a').sort
    assert_equal ["ac1", "ac2", "bc1", "bc2"], DP.longest_common_subsequence('abc12', 'bac21').sort
    assert_equal ["aba", "bab"], DP.longest_common_substring('abab', 'baba')
    assert_equal ["abacd", "dcaba"], DP.longest_common_substring('abacdfgdcaba', 'abacdgfdcaba')
    assert_equal 5, DP.longest_palindromic_subsequence('xaybzba')
    assert_equal [1, 3, 3], DP.longest_increasing_subsequence([1, 3, 3, 2])
    assert_equal [1, 2, 3], DP.longest_increasing_subsequence([7, 8, 1, 5, 6, 2, 3])
    assert_equal [1, 5, 6], DP.longest_increasing_subsequence_v2([7, 8, 1, 5, 6, 2, 3])
  end

  def test_edit_distance
    assert_equal [3, "SMMMSMI"], DP.edit('kitten', 'sitting')
  end

  def test_one_sided_binary_search # algorithm design manual 4.9.2
    # find the exact point of transition within an array that contains a run of 0's and an unbounded run of 1's.
    # return 1 if 1 == ary[1]
    # return 1..∞ { |n| break (Arrays.first_index(ary, 2 ** (n-1) + 1, 2 ** n) if 1 == ary[2 ** n]) }
  end

  def test_ebay_sales_fee
    # http://pages.ebay.com/help/sell/fees.html
    # http://www.ruby-doc.org/gems/docs/a/algorithms-0.5.0/Containers/RubyRBTreeMap.html
    # http://www.ruby-doc.org/gems/docs/a/algorithms-0.5.0/Containers/RubySplayTreeMap.html
    # the basic cost of selling an item is the insertion fee plus the final value fee.
    # * $0.5 for buy it now or fixed price format listings
    # * 7% for initial $50 (max: $3.5), 5% for next $50 - $1000 (max: $47.5), and 2% for the remaining.
    # http://docs.oracle.com/javase/6/docs/api/java/util/TreeMap.html
    # formulas = new TreeMap() {{ put($0, Pair.of($0, 7%)); put($50, Pair.of($3.5, 5%)); put($1000, Pair.of($51, 2%)) }}
    # sale = $1100; formula = formulas.floorEntry(sale);
    # fees = 0.5 /* insertion */ + formula.value().first() /* final base */ + formula.value().second() * (sale - formula.key()) /* final addition */
  end

  def test_reverse_decimal
    assert_equal 321, Numbers.reverse_decimal(123)
    assert_equal 21, Numbers.reverse_decimal(120)
    assert_equal 1, Numbers.reverse_decimal(100)
  end

#  def test_circular_buffer
#    buffer = CircularBuffer.new(3)
#    assert buffer.empty? && !buffer.full?
#    buffer.enq(10).enq(20).enq(30)
#    assert_raise(RuntimeError) { buffer.enq(40) }
#    assert buffer.full?
#
#    assert_equal 10, buffer.deq
#    assert_equal 20, buffer.deq
#    assert_equal 30, buffer.deq
#    assert_raise(RuntimeError) { buffer.deq }
#    assert buffer.empty?
#  end

  def test_non_repeated
    assert_equal 'abc', Strings.non_repeated('abc')
    assert_equal 'a', Strings.non_repeated('abcbcc')
  end

  def test_transpose_matrix_in_1d_array
    assert_equal [0, 3, 6, 1, 4, 7, 2, 5, 8], Arrays.transpose_to_v1((0...9).to_a, 3) # square matrix
    assert_equal [0, 4, 1, 5, 2, 6, 3, 7], Arrays.transpose_to((0...8).to_a, 2)
    assert_equal [0, 4, 8, 1, 5, 9, 2, 6, 10, 3, 7, 11], Arrays.transpose_to((0...12).to_a, 3)
    assert_equal [0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8, 11], Arrays.transpose_to((0...12).to_a, 4)
  end

  def test_exclusive_products
    assert_equal [120, 60, 40, 30, 24], Arrays.exclusive_products([1, 2, 3, 4, 5])
  end

  def test_partition
    assert_equal [[5]], Partitions.int_composition(5, 1)
    assert_equal [[1,4],[2,3],[3,2],[4,1]], Partitions.int_composition(5, 2)
    assert_equal [[1,1,3],[1,2,2],[1,3,1],[2,1,2],[2,2,1],[3,1,1]], Partitions.int_composition(5, 3)
    assert_equal [[1,1,1,2],[1,1,2,1],[1,2,1,1],[2,1,1,1]], Partitions.int_composition(5, 4)
    assert_equal [[1,1,1,1,1]], Partitions.int_composition(5, 5)
    assert_equal 16, Partitions.int_composition(5).size

    assert_equal [[1]], Partitions.int_partition(1)
    assert_equal [[2], [1, 1]], Partitions.int_partition(2)
    assert_equal [[3], [2, 1], [1, 1, 1]], Partitions.int_partition(3)
    assert_equal [[4], [3, 1], [2, 2], [2, 1, 1], [1, 1, 1, 1]], Partitions.int_partition(4)

    assert_equal ['{a,b}, {c}', '{a,c}, {b}', '{a}, {b,c}', '{a}, {b}, {c}'], 
      Partitions.set_partition(['a', 'b', 'c']).map {|p| p.map {|a| "{#{a.join(',')}}"}.join(', ') }

    # http://code.activestate.com/recipes/577211-generate-the-partitions-of-a-set-by-index/history/1/
    # http://oeis.org/wiki/User:Peter_Luschny/SetPartitions
    # http://en.wikipedia.org/wiki/Partition_(number_theory)
    # http://mathworld.wolfram.com/RestrictedGrowthString.html
    # http://oeis.org/wiki/User:Peter_Luschny
  end

  def test_knapsack
    skus = [[2, 2], [1, 1], [10, 4], [2, 1],  [4, 12]]
    assert_equal [36, [2, 2, 2, 3, 3, 3]], DP.knapsack_unbounded(skus, 15) # max sum = 36
    assert_equal [15, [3, 2, 1, 0]], DP.knapsack01(skus, 15) # max sum = 15

    # http://www.youtube.com/watch?v=ItF22I8f3Xs&feature=plcp
    assert_equal [1, 2, 1], DP.balanced_partition([5, 1, 2, 1])
    assert_equal [8, 4, 5], DP.balanced_partition([7, 11, 5, 4, 8])

    assert_equal [3, 0], DP.jump_game2([3, 2, 1, 0, 4])
    assert_equal [1, 3], DP.jump_game2([2, 3, 1, 1, 4])

    prices  = [1, 5, 8, 9, 10, 17, 17, 20]
    lengths = [1, 2, 3, 4,  5,  6,  7,  8]
    assert_equal [5, [2]], DP.cut_rod(prices, lengths, 2)
    assert_equal [13, [2, 3]], DP.cut_rod(prices, lengths, 5)
    assert_equal [27, [2, 2, 6]], DP.cut_rod(prices, lengths, 10)
  end

  def test_find_modes_n_missing_numbers
    assert_equal [3, 4], Arrays.find_modes_using_map([1, 2, 3, 4, 2, 3, 4, 3, 4])
    assert_equal [3, 4], Arrays.find_modes_using_array([1, 2, 3, 4, 2, 3, 4, 3, 4])
    assert_equal [], Arrays.missing_numbers([1, 2, 3])
    assert_equal [2, 4, 5], Arrays.missing_numbers([1, 3, 6])
  end

  def test_index_of_by_rabin_karp
    assert_equal 1, Strings.index_of_by_rabin_karp("aabab", "ab")
    assert_equal 2, Strings.index_of_by_rabin_karp("aaabcc", "abc")
    assert_equal 2, Strings.index_of_by_rabin_karp("aaabc", "abc")
    assert_equal 0, Strings.index_of_by_rabin_karp("abc", "abc")
    assert_equal 0, Strings.index_of_by_rabin_karp("abcc", "abc")
    assert_equal -1, Strings.index_of_by_rabin_karp("abc", "xyz")
    assert_equal -1, Strings.index_of_by_rabin_karp("abcc", "xyz")
  end

  def test_quickfind_n_mergesort
    assert_equal 1, [9, 8, 7, 6, 5, 4, 3, 2, 1, 0].quickfind_k!(1)[1]
    assert_equal 2, [9, 8, 7, 4, 5, 6, 3, 2, 1, 0].quickfind_k!(2)[2]
    assert_equal 3, [9, 8, 7, 6, 5, 4, 1, 2, 3, 0].quickfind_k!(3)[3]
    assert_equal [0, 1, 2, 3], [9, 8, 7, 6, 5, 4, 1, 2, 3, 0].quicksort_k!(3)[0..3]

    # http://en.wikipedia.org/wiki/Merge_sort
    assert_equal [0, 2, 3, 5, 6, 8, 9], Arrays.merge_sort!([3, 6, 9, 2, 5, 8, 0])
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

  def test_bracket_n_wildcard_match?
    assert !Strings.regex_match?('c', 'a')
    assert !Strings.regex_match?('aa', 'a')
    assert Strings.regex_match?("aa","aa")
    assert !Strings.regex_match?("aaa","aa")
    assert Strings.regex_match?("aa", "a*")
    assert Strings.regex_match?("aa", ".*")
    assert Strings.regex_match?("ab", ".*")
    assert Strings.regex_match?("cab", "c*a*b")
    assert Strings.wildcard_match?('q', '?')
    assert Strings.wildcard_match?('', '*')
    assert !Strings.wildcard_match?('x', '')
    assert Strings.wildcard_match?('c', '*')
    assert Strings.wildcard_match?('ba', '*')
    assert Strings.wildcard_match?('cbax', '*x')
    assert Strings.wildcard_match?('xcba', 'x*')
    assert Strings.wildcard_match?('abxcdyef', '*x*y*')
    assert Strings.bracket_match?("{}")
    assert Strings.bracket_match?("[]")
    assert Strings.bracket_match?("()")
    assert Strings.bracket_match?("[ { ( a + b ) * -c } % d ]")
  end

  def test_from_to_excel_column
    assert_equal 'ABC', Numbers.to_excel_column(731)
    assert_equal 731, Numbers.from_excel_column(Numbers.to_excel_column(731))
  end

  def test_fibonacci
    assert_equal 0, Numbers.fibonacci(0)
    assert_equal 1, Numbers.fibonacci(1)
    assert_equal 8, Numbers.fibonacci(6)
  end

  def test_manual_1_28_divide
    # 1-28. Write a function to perform integer division w/o using either the / or * operators.
    assert_equal 85, Numbers.divide(255, 3)
    assert !Numbers.power_of_2?(5)
    assert Numbers.power_of_2?(4)
    assert Numbers.power_of_2?(2)
    assert Numbers.power_of_2?(1)
    assert !Numbers.power_of_2?(0)
    assert_equal 5, Numbers.abs(-5)
    assert_equal 7, Numbers.abs(7)
    assert_equal [1, 11], Numbers.minmax(1, 11)
    assert_equal [-2, 0], Numbers.minmax(0, -2)
    assert Numbers.opposite_in_sign?(-10, 10)
    assert !Numbers.opposite_in_sign?(-2, -8)
  end

  # 1-29. There are 25 horses. At most, 5 horses can race together at a time. You must determine the fastest, second fastest, and third fastest horses. Find the minimum number of races in which this can be done.

  # 2-43. You are given a set S of n numbers. You must pick a subset S' of k numbers from S such that the probability of each element of S occurring in S' is equal (i.e., each is selected with probability k / n). You may make only one pass over the numbers. What if n is unknown?
  # 2-47. You are given 10 bags of gold coins. Nine bags contain coins that each weigh 10 grams. One bag contains all false coins that weigh one gram less. You must identify this bag in just one weighing. You have a digital balance that reports the weight of what is placed on it.
  # 2-51. Six pirates must divide $300 dollars among themselves. The division is to proceed as follows. The senior pirate proposes a way to divide the money. Then the pirates vote. If the senior pirate gets at least half the votes he wins, and that division remains. If he doesn't, he is killed and then the next senior-most pirate gets a chance to do the division. Now you have to tell what will happen and why (i.e., how many pirates survive and how the division is done)? All the pirates are intelligent and the first priority is to stay alive and the next priority is to get as much money as possible.

  # 3-21. Write a function to compare whether two binary trees are identical. Identical trees have the same key value at each position and the same structure.
  # 3-22. Write a program to convert a binary search tree into a linked list.
  # 3-28. You have an unordered array X of n integers. Find the array M containing n elements where Mi is the product of all integers in X except for Xi. You may not use division. You can use extra memory. (Hint: There are solutions faster than O(n2).)
  # 3-29. Give an algorithm for finding an ordered word pair (e.g., New York) occurring with the greatest frequency in a given webpage. Which data structures would you use? Optimize both time and space.

  def test_manual_4_45_smallest_snippet_of_k_words
    # Given a search string of three words, find the smallest snippet of the document that contains all three of 
    # the search words --- i.e. the snippet with smallest number of words in it. You are given the index positions 
    # where these words occur in search strings, such as word1: (1, 4, 5), word2: (3, 9, 10), word3: (2, 6, 15). 
    # Each of the lists are in sorted order as above.
    # http://rcrezende.blogspot.com/2010/08/smallest-relevant-text-snippet-for.html
    # http://blog.panictank.net/tag/algorithm-design-manual/
    assert_equal [117, 130], Strings.min_window([[0, 89, 130], [95, 123, 177, 199], [70, 105, 117]])
    assert_equal 'adab', Strings.min_window_string('abracadabra', 'abad')
  end

  # 4-45. Given 12 coins. One of them is heavier or lighter than the rest. Identify this coin in just three weightings.
  # http://learntofish.wordpress.com/2008/11/30/solution-of-the-12-balls-problem/

  def test_manual_7_14_permutate
    # 7-14. Write a function to find all permutations of the letters in a particular string.
    permutations = ["aabb", "abab", "abba", "baab", "baba", "bbaa"]
    assert_equal permutations, Search.permutate('aabb'.chars.to_a).map { |p| p.join }.sort
    assert_equal permutations, Search.permutation('aabb'.chars.to_a).map { |p| p.join }.sort
  end

  def test_manual_7_15_k_element_subsets
    # 7-15. Implement an efficient algorithm for listing all k-element subsets of n items.
    assert_equal ['abc'], Search.combination('abc'.chars.to_a, 3).map { |e| e.join }
    assert_equal ['ab', 'ac', 'bc'], Search.combination('abc'.chars.to_a, 2).map { |e| e.join }
    assert_equal [''], Search.combination('cba'.chars.to_a, 0).map { |e| e.join }
    assert_equal ['abb', 'ab', 'a', 'bb', 'b', ''], Search.combination('abb'.chars.to_a).map { |e| e.join }
  end

  # 7-16. An anagram is a rearrangement of the letters in a given string into a sequence of dictionary words,
  #       like Steven Skiena into Vainest Knees. Propose an algorithm to construct all the anagrams of a given string.

  # 7-17. Telephone keypads have letters on each numerical key. Write a program that generates all possible words 
  #       resulting from translating a given digit sequence (e.g., 145345) into letters.

  # 7-18. You start with an empty room and a group of n people waiting outside. At each step, 
  #       you may either admit one person into the room, or let one out. Can you arrange a sequence of 2n steps,
  #       so that every possible combination of people is achieved exactly once?

  # 7-19. Use a random number generator (rng04) that generates numbers from {0, 1, 2, 3, 4} with equal probability 
  #       to write a random number generator that generates numbers from 0 to 7 (rng07) with equal probability.
  #       What are expected number of calls to rng04 per call of rng07?

  # 8-24. Given a set of coin denominators, find the minimum number of coins to make a certain amount of change.
  def test_manual_8_24_make_change
    assert_equal [], DP.minimal_coins(0, [1, 5, 7])
    assert_equal [5, 5], DP.minimal_coins(10, [1, 5, 7])
    assert_equal [1, 5, 7], DP.minimal_coins(13, [1, 5, 7])
    assert_equal [7, 7], DP.minimal_coins(14, [1, 5, 7])
  end

  # 8-25. You are given an array of n numbers, each of which may be positive, negative,
  #       or zero. Give an efficient algorithm to identify the index positions i and j to the
  #       maximum sum of the ith through jth numbers.
  # 8-26. Observe that when you cut a character out of a magazine, the character on the
  #       reverse side of the page is also removed. Give an algorithm to determine whether
  #       you can generate a given string by pasting cutouts from a given magazine. Assume
  #       that you are given a function that will identify the character and its position on
  #       the reverse side of the page for any given character position.

  def test_3_4_hanoi
    Arrays.move_tower('A', 'C', 'B', 3) # from 'A' to 'C' via 'B'.
  end

  def test_3_6_sort_by_stack
    assert_equal [-4, -3, 1, 2, 5, 6], Arrays.sort_using_stack!([5, -3, 1, 2, -4, 6])
  end

  def test_9_3_min_n_index_out_of_cycle
    assert_equal 10, Arrays.index_out_of_cycle([10, 14, 15, 16, 19, 20, 25, 1, 3, 4, 5, 7], 5)
    assert_equal 7, Arrays.index_out_of_cycle([16, 19, 20, 25, 1, 3, 4, 5, 7, 10, 14, 15], 5)
    assert_equal 0, Arrays.index_out_of_cycle([5, 7, 10, 14, 15, 16, 19, 20, 25, 1, 3, 4], 5)
    assert_equal 5, Arrays.index_out_of_cycle([20, 25, 1, 3, 4, 5, 7, 10, 14, 15, 16, 19], 5)

    assert_equal 6, Arrays.min_out_of_cycle([6])
    assert_equal 6, Arrays.min_out_of_cycle([6, 7])
    assert_equal 6, Arrays.min_out_of_cycle([7, 6])
    assert_equal 6, Arrays.min_out_of_cycle([38, 40, 55, 89, 6, 13, 20, 23, 36])
    assert_equal 6, Arrays.min_out_of_cycle([6, 13, 20, 23, 36, 38, 40, 55, 89])
    assert_equal 6, Arrays.min_out_of_cycle([13, 20, 23, 36, 38, 40, 55, 89, 6])

    assert_equal 5, Arrays.last_index([1, 3, 3, 5, 5, 5, 7, 7, 9], 5)
    assert_equal 3, Arrays.first_index([1, 3, 3, 5, 5, 5, 7, 7, 9], 5)
    assert_equal 1..2, Arrays.find_occurences([1, 3, 3, 5, 5, 5, 7, 7, 9], 3)
    assert_equal 6..7, Arrays.find_occurences([1, 3, 3, 5, 5, 5, 7, 7, 9], 7)
    assert_equal 0..0, Arrays.find_occurences([1, 3, 3, 5, 5, 5, 7, 7, 9], 1)
    assert_equal nil, Arrays.find_occurences([1, 3, 3, 5, 5, 5, 7, 7, 9], 0)
    assert_equal nil, Arrays.find_occurences([1, 3, 3, 5, 5, 5, 7, 7, 9], 10)
  end

  def test_9_6_indexes_out_of_matrix
    m = [
      [11, 23, 35, 47],
      [22, 34, 38, 58],
      [33, 39, 57, 62],
      [44, 45, 61, 69]
    ]

    assert_equal [0, 3], Arrays.indexes_out_of_matrix(m, 47)
    assert_equal [3, 3], Arrays.indexes_out_of_matrix(m, 69)
    assert_equal [0, 0], Arrays.indexes_out_of_matrix(m, 11)
    assert_equal [3, 0], Arrays.indexes_out_of_matrix(m, 44)
    assert_equal [2, 1], Arrays.indexes_out_of_matrix(m, 39)
    assert_equal [3, 2], Arrays.indexes_out_of_matrix(m, 61)
  end

  def test_10_4_arithmetic_operations
    # 10-4. Write a method to implement *, - , / operations. You should use only the + operator.
    assert_equal -7, Math.negate(7)
    assert_equal 0, Math.negate(0)
    assert_equal -4, Math.subtract(3, 7)
    assert_equal 10, Math.subtract(3, -7)
    assert_equal -21, Math.multiply(3, -7)
    assert_equal 21, Math.multiply(-3, -7)
    assert_equal -3, Math.divide(11, -3)
    assert_equal 3, Math.divide(-11, -3)
  end

  def test_10_5_line_of_cutting_two_squares
    # 10-5. Given two squares on a two dimensional plane, find a line that would cut these two squares in half.
    r1, r2 = [4, 0, 0, 6], [6, 0, 0, 4] # top, left, bottom, right
    center_of = proc { |r| [(r[1] + r[3])/2.0, (r[0] + r[2])/2.0] }
    center1, center2 = center_of[r1], center_of[r2]
    line_of = proc { |p, q|
      x, y = p[0] - q[0], p[1] - q[1]
      case
      when x == 0 && y == 0 then nil
      when x == 0 then [p[0], nil]
      when y == 0 then [nil, p[1]]
      else
        s = y * 1.0 / x # scope
        [p[0] - p[1] * 1/s, p[1] - p[0] * s]
      end
    }
    assert_equal [5.0, 5.0], line_of[center1, center2]
  end

  def test_10_3_n_10_6_line_of_most_points
    # 10-3. Given two lines on a Cartesian plane, determine whether the two lines'd intersect.
    # 10-6. Given a two dimensional graph with points on it, find a line which passes the most number of points.
    a = [[1, 2], [2, 4], [6, 12], [3, 2], [4, 0], [3, 2], [5, -2]]
    n = a.size
    h = {}
    line_of = proc do |p, q|
      x, y = p[0] - q[0], p[1] - q[1]
      case
      when x == 0 && y == 0 then nil
      when x == 0 then [p[0], nil]
      when y == 0 then [nil, p[1]]
      else
        s = y * 1.0 / x # scope
        [p[0] - p[1] * 1/s, p[1] - p[0] * s]
      end
    end

    assert_equal [1.75, -7.0], line_of[[3, 5], [2, 1]]
    (0...n).each do |i|
      (i+1...n).each do |j|
        line = line_of[a[i], a[j]]
        (h[line] ||= {})[a[i]] = 1;
        (h[line] ||= {})[a[j]] = 1;
      end
    end

    h = h.values.
      reject { |s| s.size == 2 }.
      map { |h| h.keys }.
      map { |z| [z.size, z] }
    h = h.group_by { |e| e[0] }.max.last
    assert_equal [[4, [[2, 4], [3, 2], [4, 0], [5, -2]]]], h
  end

  def test_10_7_kth_number_of_prime_factors
    # 10-7. Design an algorithm to find the kth number such that the only prime factors are 3, 5, and 7.
    
  end

  # 11-7. In a B+ tree, in contrast to a B-tree, all records are stored at the leaf level of the tree; only keys are stored in interior nodes.
  #       The leaves of the B+ tree in a linked list makes range queries or an (ordered) iteration through the blocks simpler & more efficient.

  # 13-2. hashmap vs. treemap that is a Red-Black tree based NavigableMap implementation that provides guaranteed log(n) time cost for operations.
  # 13-3. virtual functions & destructors, overloads, overloads, name-hiding, and smart pointers in C++?

  # RFC 2581: slow start, congestion avoidance, fast retransmit and recovery http://www.ietf.org/rfc/rfc2581.txt, http://msdn.microsoft.com/en-us/library/ms819737.aspx
  # Slow Start: When a connection is established, TCP starts slowly at first so as to assess the bandwidth of the connection and
  #   to avoid overflowing the receiving host or any other devices or links in the connection path. 
  #   If TCP segments are acknowledged, the window size is incremented again, and so on 
  #   until the amount of data being sent per burst reaches the size of the receive window on the remote host.
  # Congestion Avoidance: If the need to retransmit data happens, the TCP stack acts under the assumption that network congestion is the cause.
  #   The congestion avoidance algorithm resets the receive window to half the size of the send window at the point when congestion occurred.
  #   TCP then enlarges the receive window back to the currently advertised size more slowly than the slow start algorithm.
  # Fast Retransmit & Recovery: To help make the sender aware of the apparently dropped data as quickly as possible,
  #   the receiver immediately sends an acknowledgment (ACK), with the ACK number set to the sequence number that seems to be missing.
  #   The receiver sends another ACK for that sequence number for each additional TCP segment in the incoming stream 
  #   that arrives with a sequence number higher than the missing one.

  def test_18_3_singleton
    # double-check locking
  end

  def test_19_2_is_tic_tac_toe_over
    # Design an algorithm to figure out if someone has won in a game of tic-tac-toe.
  end

  def test_19_7_maxsum_subarray
    assert_equal [5, [1, 7]], Arrays.maxsum_subarray([-2, 1, 3, -3, 4, -2, -1, 3])
  end

  def test_19_10_test_rand7
    100.times { r = Numbers.rand7; raise "'r' must be 1..7." unless r >= 1 && r <= 7 }
  end

  def test_19_11_pairs_of_sum
    pairs = Arrays.pairs_of_sum([1, 2, 1, 5, 5, 5, 3, 9, 9, 8], 10)
    expected = [[5, 5], [1, 9], [2, 8]]
    assert expected.eql?(pairs)
  end

  def test_20_1_addition
    assert_equal 1110 + 323, Numbers.sum(1110, 323)
  end

  def test_20_2_knuth_shuffle
    ary = Numbers.knuth_suffle!((1..52).to_a) # shuffles a deck of 52 cards
    assert_equal 52, ary.size
  end

  def test_20_3_reservoir_samples_n_weighted_choice
    io = StringIO.new("a\nb\nc\nd\ne\nf\ng\nh\ni\nj\nk\n")
    assert_equal 3, Numbers.reservoir_samples(io, 3).size
    assert -1 < Numbers.weighted_choice([10, 20, 30, 40])
  end

  def test_20_7_find_longest_compound_words
    # Given a list of words, write a program that returns the longest word made of other words.
    # e.g. return "doityourself" given a list, "doityourself", "do", "it", "yourself", "motherinlaw", "mother", "in", "law".
    w = %w(approximation do it yourself doityourself motherinlaw mother in law).sort_by { |s| -s.size }
    d = w.reduce({}) { |h,k| h.merge(k => nil) }
    s = w.detect do |word|
      Partitions.int_composition(word.size, 2..3).any? do |composition|
        prefix_sums = composition.reduce([]) { |a,e| a + [e + (a.last || 0)] }
        words = (0...prefix_sums.size).map { |j| word[(j > 0 ? prefix_sums[j-1] : 0)...prefix_sums[j]] }
        words.all? { |k| d.has_key?(k) }
      end
    end

    assert_equal 'doityourself', s
  end

  def test_20_10_trans_steps_of_2_words
    # Given two words of equal length that are in a dictionary, 
    # design a method to transform one word into another word by changing only one letter at a time.
    # The new word you get in each step must be in the dictionary.
    d = %w{CAMP DAMP LAMP RAMP LIMP LUMP LIMO LITE LIME LIKE}.reduce({}) { |h, k| h.merge(k => nil) }
    reproduced = {}
    solutions = []
    branch_out = lambda do |a|
      candidates = []
      a.last.size.times do |i|
        s = a.last.dup
        ('A'..'Z').each do |c|
          s[i] = c
          if !reproduced.has_key?(s) && d.has_key?(s)
            candidates.push(s.dup)
            reproduced[candidates.last] = nil
          end
        end
      end
      candidates
    end

    reduce_off = lambda do |a|
      solutions.push(a.dup) if a.last.eql?('LIKE')
    end

    reproduced['DAMP'] = nil
    Search.backtrack(['DAMP'], branch_out, reduce_off)
    assert_equal ['DAMP', 'LAMP', 'LIMP', 'LIME', 'LIKE'], solutions.last
  end

  def test_20_11_max_size_subsquare
    # Imagine you have a square matrix, where each cell is filled with either black (1) or white (0).
    # Design an algorithm to find the maximum sub-square such that all four borders are filled with black pixels.
    m = [
      [0, 1, 1, 0, 1, 0],
      [1, 1, 1, 1, 0, 1],
      [1, 1, 0, 1, 1, 0],
      [1, 0, 1, 1, 1, 1],
      [0, 1, 1, 1, 1, 1],
      [1, 0, 1, 1, 1, 0]
    ]
    assert_equal [3, [3, 2]], Arrays.max_size_subsquare(m)
  end

  def test_20_12_maxsum_submatrix
    m = [ # 4 x 3 matrix
      [ 1,  0, 1], 
      [ 0, -1, 0], 
      [ 1,  0, 1], 
      [-5,  2, 5]
    ]
    assert_equal [8, [2, 1, 3, 2]], Arrays.maxsum_submatrix(m)
    assert_equal [3, [0, 0, 0, 1]], Arrays.maxsum_submatrix([[1, 2, -1], [-3, -1, -4], [1, -5, 2]])
  end
end

###########################################################
# Miscellaneous Modules & Classes: SNode, and DNode
###########################################################

module Kernel
  def enum(*symbols)
    symbols.each { |s| const_set(s, s.to_s) }
    const_set(:DEFAULT, symbols.first) unless symbols.nil?
  end
end

###########################################################
# System and Object-Oriented Design
###########################################################

module JukeboxV1
  # a design consists of key components & their responsibilities, interactions, and services.
  # http://alistair.cockburn.us/Coffee+machine+design+problem,+part+2
  # http://codereview.stackexchange.com/questions/10700/music-jukebox-object-oriented-design-review
  # http://www.simventions.com/whitepapers/uml/3000_borcon_uml.html
  class JukeboxEngine # coordinates all activities.
    def play(track) end; def choose_track() end; def show_welcome() end
    attr_reader :current_account, :current_track
    attr_reader :now_playing, :most_played, :recently_played, :recently_added
    attr_reader :account_dao, :album_dao, :playlist_dao
  end

  class CardReader # reads magnetic account id cards.
    def load_account() end
  end

  class Playlist # keeps track of most-played, recently-played, and now-playing lists.
    def initialize(capacity, mode = :LRU_CACHE) end
    def enqueue(track) end # reordering occurs in LRU-CACHE mode.
    def dequeue() end # bubbling down happens in MIN-HEAP mode.
    def move_up(track) end
    def move_down(track) end
    def size() end
  end

  class AudioCDPlayer # controls actual audio CD player.
    def play(track) end
    def pause() end
    def resume() end
    def stop() end
    def set_volume() end
    def get_volume() end
    def mute() end
    def unmute() end
  end

  class Account # keeps track of user account.
    attr_accessor :credit, :most_played, :recent_played
  end

  class Album # location: a certain tray id.
    attr_reader :label, :artists, :release_date, :genre, :subgenre, :tracks, :location
  end

  class Track # location: a certain track id.
    attr_reader :title, :duration, :location
  end

  class AccountDAO # (same as AlbumDAO and PlaylistDAO)
    def save(entity) end # create and update
    def find(id) end
    def find_all(example = nil) end
    def count(example = nil) end
    def exists?(example = nil) end
    def delete(id) end
    def delete_all(example = nil) end # delete
  end
end

module CardGame
  # http://math.hws.edu/javanotes/c5/s4.html
  class Card
    attr_accessor :suit, :value
  end

  class Deck
    def suffle() end
    def cards_left() end
    def deal_card() end # raises illegal state.
  end

  class Hand
    def add_card(card) end
    def remove_card(card) end # raises no such item found.
    def remove_card(position) end # raises index out of bounds.
    def get_card(position) end
    def clear_cards() end
    def sort_by_suit() end
    def sort_by_value() end
    attr_accessor :cards
  end

  class Game
    def games_played() end
    def scores_achieved() end
    def set_up() end
    def tear_down() end
    attr_accessor :deck, :hands
  end
end

module TetrisV1
  class TetrisEngine
    def set_up() end; def tear_down() end; def clear() end
    def move_piece() end; def update_state() end; def remove_lines; end
    attr_reader :current_piece, :pieces_in_queue, :board_array, :game_state
    attr_reader :current_level, :current_score, :games_palyed, :best_scores
  end

  class TetrisBlock < Struct.new(:shape, :color, :orientation, :location); end

  module TetrisShape # http://www.freewebs.com/lesliev/rubyenums.html
    enum :SQUARE, :LINE, :T, :L, :REVERSE_L, :Z, :REVERSE_Z
  end
end

module XMPP
  # http://en.wikipedia.org/wiki/Extensible_Messaging_and_Presence_Protocol
  # http://weblog.raganwald.com/2006/06/my-favourite-interview-question.html
  # http://www.12planet.com/en/software/chat/whatis.html
  # http://tianrunhe.wordpress.com/2012/03/21/design-an-online-chat-server/
  # http://tianrunhe.wordpress.com/tag/object-oriented-design/
  # http://tianrunhe.wordpress.com/2012/03/page/2/
  # http://www.thealgorithmist.com/archive/index.php/t-451.html?s=b732f53954d644b5ebacc77460396c19
  # https://www.google.com/#q=interview+chat+server+design&hl=en&prmd=imvns&ei=lhM3UK33A8SQiQLO-4CAAQ&start=90&sa=N&bav=on.2,or.r_gc.r_pw.r_cp.r_qf.&fp=f0056a833a456726&biw=838&bih=933
end
