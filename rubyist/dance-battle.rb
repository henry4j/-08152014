#!/usr/bin/env ruby

# http://clickedyclick.blogspot.com/2011/01/facebook-haker-cup-round-1a-that-wasnt.html
# http://www.kelvinjiang.com/2010/10/facebook-puzzles-dance-battle.html
# http://en.wikipedia.org/wiki/Dijkstra's_algorithm#Pseudocode

%w{test/unit open-uri}.each { |e| require e }

class Graph
  def self.dijkstra(s, each_vertex, each_edge)
    parents = {}
    distances = Hash.new(Float::MAX).merge(s => 0)
    q = BinaryHeap.new(proc { |a, b| a[1] <=> b[1] }, proc { |e| e[0] })
    each_vertex[proc { |v| q.offer([v, Float::MAX]) }]
    q.offer([s, 0])
    until q.empty? || Float::MAX == q.peek[1]
      each_edge[u = q.poll[0], proc { |v, w|
        via_u = distances[u] + w
        if via_u < distances[v]
          q.offer([v, distances[v] = via_u])
          parents[v] = u
        end
      }]
    end
    parents
  end
end

class BinaryHeap # min-heap by default, http://en.wikipedia.org/wiki/Binary_heap
  # http://docs.oracle.com/javase/7/docs/api/java/util/PriorityQueue.html
  # a binary heap is a complete binary tree, where all levels but the last one are fully filled, and
  # each node is smaller than or equal to each of its children according to a comparer specified.
  def initialize(comparer = proc { |a, b| a <=> b }, hash = proc { |e| e.hash }) # min-heap by default
    @a = []
    @h = {}
    @comparer = comparer
    @hash = hash
  end

  def offer(e)
    n = @h[@hash[e]]
    if n
      @a[n] = e
      if n == bubble_up(n)
        bubble_down(n)
      end
    else
      @a << e
      bubble_up(@a.size - 1)
    end
    self # works as a fluent interface.
  end

  def peek
    @a[0]
  end

  def poll
    unless @a.empty?
      @a[0], @a[-1] = @a[-1], @a[0]
      head = @a.pop
      bubble_down(0) unless @a.empty?
      head
    end
  end

  def bubble_up(n)
    if n > 0 && @comparer.call(@a[p = (n-1)/2], @a[n]) > 0
      @a[p], @a[n] = @a[n], @a[p]
      @h[@hash[@a[n]]] = n
      bubble_up(p)
    else
      @h[@hash[@a[n]]] = n
    end
  end

  def bubble_down(n)
    c = [n]
    c << 2*n + 1 if 2*n + 1 < @a.size
    c << 2*n + 2 if 2*n + 2 < @a.size
    c = c.min { |a,b| @comparer.call(@a[a], @a[b]) }
    if c != n
      @a[n], @a[c] = @a[c], @a[n]
      @h[@hash[@a[n]]] = n
      bubble_down(c)
    else
      @h[@hash[@a[n]]] = n
    end
  end

  def empty?() @a.empty? end
  def size() @a.size end
  def to_a() @a end
end

class TestCases < Test::Unit::TestCase
  def test_dance_battle
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/dance-battle-testcases/input00.txt'
    open(test_case_uri) do |io|
      (Integer(io.readline.chomp)).times do
        h, w, *m = io.readline.split(' ')
        h, w = h.to_i, w.to_i
        m = m.map { |l| l.split('') }
        portals = m.each_with_index.reduce({}) do |q, (e, r)| 
          e.each_with_index.reduce(q) do |q, (e, c)| 
            (q[e] ||= []) << [r, c] if m[r][c].to_i > 0; q
          end
        end
        each_edge = proc do |u, blk|
          (r, c) = u
          ([[r-1, c], [r+1, c], [r, c-1], [r, c+1]].reject { |(p, q)|
            p < 0 || q < 0 || p >= h || q >= w || 'W' == m[p][q]
          } + (portals[m[r][c]] || []) - [[r, c]]).each { |v|
            blk[v, 1]
          }
        end
        each_vertex = proc do |blk|
          m.each_index { |r| m[r].each_index { |c| blk[[r, c]] } }
        end

        s = [0, m[0].index('S')]
        e = [h-1, m[h-1].index('E')]
        parents = Graph.dijkstra(s, each_vertex, each_edge)
        path = [e]
        path.unshift(parents[path[0]]) until s == parents[path[0]]
        puts (path.size)
      end
    end
  end

  def test_priority_heap
  h = BinaryHeap.new(proc { |a, b| b[1] <=> a[1] }, proc { |e| e[0] })
    h.offer(['d', 10]).offer(['e', 30]).offer(['h', 50]).
      offer(['f', 20]).offer(['b', 40]).offer(['c', 60]).
      offer(['a', 80]).offer(['i', 90]).offer(['g', 70])
    h.offer(['a', 92]).offer(['b', 98]).offer(['h', 120])
    h.offer(['i', 45]).offer(['c', 25])
    assert_equal ["h", 120], h.peek
    assert_equal ["h", 120], h.poll
    assert_equal ["b", 98], h.poll
    assert_equal ["a", 92], h.poll
    assert_equal ["g", 70], h.poll
    assert_equal ["i", 45], h.poll
    assert_equal ["e", 30], h.poll
    assert_equal ["c", 25], h.poll
    assert_equal ["f", 20], h.poll
    assert_equal ["d", 10], h.poll
    assert_equal nil, h.poll
  end
end

=begin

Dance Battle

Walking home in triumph after a particularly satisfying dance battle, you take a wrong turn into an alley and find yourself presented with a perplexing puzzle. The entrance to the alley behind you disappears and you are faced with a rectangular grid of colorful squares, with thick granite walls occupying some of the squares. The grid is recessed far enough into the ground that, from above, you can see the exit in the distance and the layout of all the colors. You can memorize the entire layout before entering the grid and trying to make your way across.

As you stand above the grid trying to commit its layout to memory, you notice something strange about the urban wildlife trapped with you in the maze; the various fauna seem to be able to move almost instantaneously between squares of the same color. You suspect that you can use this property to your advantage, which you attempt to keep at the front of your mind as you enter the labyrinth with your intentions set on getting out as quickly as possible.

INPUT

Your input file will consist of a number N followed by some whitespace and then N test cases. Each case consists of two numbers R and C, the number of rows and columns in the maze, followed by R strings describing the layout of the rows in order. All tokens are whitespace-separated.

Each element describing a row will consist of the characters 'S', 'E', 'W', or the digits 0-9. There will be only one 'S' square and one 'E' square, 'S' being your starting point and 'E' being the exit. 'S' will be in the first row, 'E' will be in the last row. 'W' squares are occupied by walls, and cannot be entered. Digits 1-9 represent the colors of the squares. You can pass in a single step between adjacent squares or between any two squares of the same color. The start and exit points as well as all '0' squares are inert, i.e., they have no color and must be stepped directly on to or off of. There will always be some valid method of reaching the exit.

OUTPUT

Return the number of steps required to reach the exit from the starting point.

CONSTRAINTS

10 <= N <= 50 2 <= R, C <= 100
Example input

5
5 3 00S 02W 009 W50 E0W 
10 6 000S0W 00000W 000WW0 000WWW W0400W 0000WW 0300WW W000WW 0000WW WE0000 
15 9 00W0000S0 0000W0000 0WW000000 00W000000 000050000 000080005 050000000 000000000 W0W00W000 00WW0WW00 WWWW00000 0W0090500 WWW0W0000 WW00WW000 0W0WWE000 
20 12 40000000S00W 000000000000 03000000000W 100000008000 000000000000 000000700000 000000000700 000000000000 070000000000 000000002070 000100000000 000000000430 000000000000 000004004000 350000000000 W02010000030 W10000000000 000100000300 000009000000 0E0000005000 
25 15 0WS00W0W600WWW0 0W6W00WWW0WW0W0 WW0W00W0WW0WWWW W0WW04WWWW0WW0W WW0W0000WWWWWW0 000WWW000WWW9W0 W0WWWW00000WWW0 0WWW0WW0W00WW90 WWW000WW0000WWW W0W000WW0000WWW 0W0800WW0W00000 W000W0W4WW00WWW 000000WW00000WW W000030000304W0 W00000W0000000W W08000000000000 WW00WW00W0W0400 000W0W00300W050 00WWWWWW0WWWW40 00WWWWWWWW00WW0 00WW0000W000WW0 000000000000WWW W602000000W05W0 090000000000W0W WWWWWW000000EW0 

Example output

6
11
11
15
13

=end
