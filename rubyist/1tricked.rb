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

###########################################################
# Test Cases of algorithm design manual & exercises
###########################################################

class TestCases < Test::Unit::TestCase
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
