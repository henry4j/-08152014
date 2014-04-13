#!/usr/bin/env ruby

# k-d tree C-extension https://github.com/ghazel/kdtree, http://gurgeous.wordpress.com/2009/10/22/ruby-nearest-neighbor-fast-kdtree-gem/
# k-d tree C-extension thread-safe https://github.com/consti/tupalo-kdtree
# pure Ruby k-d tree https://github.com/MagLev/maglev/blob/master/examples/persistence/kdtree/lib/treekd.rb

# http://kanwei.com/code/2009/02/18/facebook-smallworld.html
# http://www.rubydoc.info/github/kanwei/algorithms/master/Algorithms
# http://www.kelvinjiang.com/2011/02/facebook-puzzles-its-small-world.html

%w{test/unit open-uri}.each { |e| require e }

class KDTree
  def nearest_k(query, k, mins = BinaryHeap.new(proc { |a, b| b[1] <=> a[1] }), reduce = true)
    mins.offer([point, point.distance_sq(query)]) # this max heap keeps track of minimums.
    mins.poll while mins.size > k

    r = point[@axis] - query[@axis] # r is the radius; the distance from the center (query point) of a circle.
    nearer, further = r > 0 ? [@left, @right] : [@right, @left]
    nearer and nearer.nearest_k(query, k, mins, false)
    further and (mins.size < k or r**2 < mins.peek[1]) and further.nearest_k(query, k, mins, false)
    reduce and [].tap { |m| m.unshift(mins.poll[0]) until mins.empty? }
  end

  def initialize(points, dimension = 2, depth = 0)
    unless points.empty?
      @axis = depth % dimension
      mid = points.size / 2
      points = points.dup if depth == 0
      points.quickfind_k!(mid) { |a, b| a[@axis] <=> b[@axis] }
      @point = points[mid]
      @left = KDTree.new(points[0...mid], dimension, depth + 1) if 0 < mid
      @right = KDTree.new(points[1+mid...points.size], dimension, depth + 1) if mid+1 < points.size
    end
  end

  attr_accessor :left, :right, :point
end

class KDPoint
  def distance_sq(other)
    @tuple.each_index.reduce(0) {|s, i| s+= (other.tuple[i] - @tuple[i]) ** 2 }
  end

  def [](index)
    @tuple[index]
  end

  def to_s
    "#{data}@(#{tuple.join(', ')})"
  end

  def initialize(tuple, data = nil)
    @tuple, @data = tuple, data
    @x = tuple[0]
    @y = tuple[1]
    @z = tuple[2]
  end

  attr_reader :tuple, :data, :x, :y, :z
end

class TestCases < Test::Unit::TestCase
  def test_small_world
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/small-world-testcases/input00.txt'
    points = []
    open(test_case_uri) do |io|
      until io.eof?
        n, x, y = io.readline.split
        points << KDPoint.new([x.to_f, y.to_f], n.to_i)
      end
    end

    tree = KDTree.new(points)
    points.each do |e|
      nearest_4 = tree.nearest_k(e, 4)
      nearest_4.shift
      puts "#{e.data} #{nearest_4.map{ |p| p.data}.join(',') }"
    end
  end

  def test_quickfind
    a = [9, -7, 5, -3, 1, 8, 6, 4, 2]
    a = a.quickfind_k!(1) { |l, r| l.abs <=> r.abs }
    assert_equal 2, a[1]
    assert_equal -3, a[2]

    a.quickfind_k!(1)
    assert_equal -3, a[1]

    s = ['microsoft', 'abc', 'mn', 'x', 'wxyz', 'facebook', 'amazon', 'bingo!!']
    s.quicksort_k!(4) { |a, b| a.size <=> b.size }
    assert_equal ["x", "mn", "abc", "wxyz", "amazon"], s[0..4]
  end
end

###########################################################
# https://facebook.interviewstreet.com/recruit/challenges
###########################################################

@challenge = <<HERE

Given a list of points in the plane, write a program that outputs each point along with the three other points that are closest to it. These three points ordered by distance.

For example, given a set of points where each line is of the form: ID x-coordinate y-coordinate

1  0.0      0.0
2  10.1     -10.1
3  -12.2    12.2
4  38.3     38.3
5  79.99    179.99


Your program should output:

1 2,3,4
2 1,3,4
3 1,2,4
4 1,2,3
5 4,3,1

HERE

###########################################################
# Utilities: BinaryHeap and Array
###########################################################

class BinaryHeap # min-heap by default, http://en.wikipedia.org/wiki/Binary_heap
  # http://docs.oracle.com/javase/7/docs/api/java/util/PriorityQueue.html
  # a binary heap is a complete binary tree, where all levels but the last one are fully filled, and
  # each node is smaller than or equal to each of its children according to a comparer specified.
  def initialize(comparer = proc { |a, b| a <=> b }) # min-heap by default
    @heap = []
    @comparer = comparer
  end

  def offer(e)
    @heap << e
    bubble_up(@heap.size - 1)
    self # works as a fluent interface.
  end

  def peek
    @heap[0]
  end

  def poll
    unless @heap.empty?
      @heap[0], @heap[-1] = @heap[-1], @heap[0]
      head = @heap.pop
      bubble_down(0)
      head
    end
  end

  def bubble_up(n)
    if n > 0
      p = (n-1)/2 # p: parent
      if @comparer.call(@heap[p], @heap[n]) > 0
        @heap[p], @heap[n] = @heap[n], @heap[p]
        bubble_up(p)
      end
    end
  end

  def bubble_down(n)
    if n < @heap.size
      c = [n]
      c << 2*n + 1 if 2*n + 1 < @heap.size
      c << 2*n + 2 if 2*n + 2 < @heap.size
      c = c.min {|a,b| @comparer.call(@heap[a], @heap[b])}
      if c != n
        @heap[n], @heap[c] = @heap[c], @heap[n]
        bubble_down(c)
      end
    end
  end

  def empty?() @heap.empty? end
  def size() @heap.size end
  def to_a() @heap end
end

class Array
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