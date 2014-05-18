#!/usr/bin/env ruby

%w{test/unit stringio set}.each { |e| require e }

class Graph
  def self.max_flow(source, sink, edges, capacites)
    # http://en.wikipedia.org/wiki/Edmonds-Karp_algorithm
    # http://en.wikibooks.org/wiki/Algorithm_Implementation/Graphs/Maximum_flow/Edmonds-Karp
    paths = []
    flows = Array.new(edges.size) { Array.new(edges.size, 0) }
    loop do
      residuals = [] # residual capacity minima.
      residuals[source] = Float::MAX
      parents = []
      entered = []
      enter_v_iff = lambda { |v| entered[v] = true if !entered[sink] && !entered[v] && residuals[v] }
      cross_e = lambda do |e, x|
        residual = capacites[x][e.y] - flows[x][e.y]
        if !entered[sink] && !entered[e.y] && residual > 0
          parents[e.y] = x
          residuals[e.y] = [ residuals[x], residual ].min
        end
      end
      BFS(source, edges, enter_v_iff, nil, cross_e)
      if parents[sink]
        path = [v = sink]
        while parents[v]
          u = parents[v]
          flows[u][v] += residuals[sink]
          flows[v][u] -= residuals[sink]
          path.unshift(v = u)
        end
        paths << [residuals[sink], path]
      else
        break;
      end
    end
    paths
  end

  def self.prim(s, edges)
    parents = []
    distances = []
    distances[s] = 0
    q = BinaryHeap.new(lambda { |a, b| a[1] <=> b[1] }, lambda { |e| e[0] }) # e[0] has v; [1] has a distance.
    q.offer([s, 0])
    until q.empty? || q.peek[1].nil?
      u, d = q.poll
      edges[u].each do |v, w|
        via_u = w
        if distances[v].nil? || via_u < distances[v]
          q.offer([v, distances[v] = via_u])
          parents[v] = u
        end
      end
    end
    parents
  end

  def self.dijkstra(s, edges)
    # http://en.wikipedia.org/wiki/Dijkstra's_algorithm#Pseudocode
    # http://www.codeproject.com/Questions/294680/Priority-Queue-Decrease-Key-function-used-in-Dijks
    parents = []
    distances = []
    distances[s] = 0
    q = BinaryHeap.new(lambda { |a, b| a[1] <=> b[1] }, lambda { |e| e[0] }) # e[0] has v; [1] has a distance.
    q.offer([s, 0])
    until q.empty? || q.peek[1].nil?
      u, d = q.poll
      edges[u].each do |v, w|
        via_u = distances[u] + w
        if distances[v].nil? || via_u < distances[v]
          q.offer([v, distances[v] = via_u])
          parents[v] = u
        end
      end
    end
    parents
  end

  def self.dijkstra_v2(s, each_vertex, each_edge)
    parents = {}
    distances = Hash.new(Float::MAX).merge(s => 0)
    q = BinaryHeap.new(lambda { |a, b| a[1] <=> b[1] }, lambda { |e| e[0] }) # e[0] has v; [1] has a distance.
    each_vertex.call(lambda { |v| q.offer([v, Float::MAX]) })
    q.offer([s, 0])
    until q.empty? || q.peek[1] == Float::MAX
      each_edge[u = q.poll[0], lambda do |v, w|
        via_u = distances[u] + w
        if via_u < distances[v]
          q.offer([v, distances[v] = via_u])
          parents[v] = u
        end
      end]
    end
    parents
  end

  def self.has_cycle?(edges, directed)
    edges.each_index.any? do |v|
      entered = []
      exited = []
      tree_edges = [] # keyed by children; also called parents map.
      back_edges = [] # keyed by ancestors, or the other end-point.
      enter = lambda { |v| entered[v] = true if not entered[v] }
      exit = lambda { |v| exited[v] = true }
      cross = lambda do |e, x|
        if not entered[e.y]
          tree_edges[e.y] = x 
        elsif (!directed && tree_edges[x] != e.y) || (directed && !exited[x])
          (back_edges[e.y] ||= []) << x # x = 1, e.y = 0
        end
      end
      Graph.DFS(v, edges, enter, nil, cross)
      !back_edges.empty?
    end
  end

  def self.topological_sort(edges)
    sort = []
    entered = []
    enter_v_iff = lambda { |v| entered[v] = true if not entered[v] }
    exit_v = lambda { |v| sort << v }
    edges.size.times do |v|
      Graph.DFS(v, edges, enter_v_iff, exit_v) unless entered[v]
    end
    sort
  end

  def self.find_all(v, edges)
    all = {}
    entered = []
    enter_v_iff = lambda { |v| entered[v] = true if not entered[v] }
    cross_e = lambda { |e, x| all[e.y] = true }
    Graph.DFS(v, edges, enter_v_iff, nil, cross_e)
    all.keys
  end

  def self.color_vertex(graph)
    answers = []
    expand_out = lambda do |a|
      v = a.size # vertex v
      c = 0..a.max # existing colors
      c = c.select { |c|
        (0...v).all? { |w| (0 == graph[v][w]) or (c != a[w]) }
      } # existing legal colors
      c + [a.max+1] # a new color.
    end
    reduce_off = lambda do |a|
      answers << [a.max+1, a.dup] if a.size == graph.size
    end
    Search.backtrack([0], expand_out, reduce_off)
    answers.min_by { |e| e[0] }
  end

  def self.navigate(v, w, edges)
    paths = []
    entered = {}
    expand_out = lambda do |a|
      entered[a[-1]] = true
      edges[a[-1]].select { |e| not entered[e.y] }.map { |e| e.y }
    end
    reduce_off = lambda do |a|
      paths << a.dup if a[-1] == w
    end
    Search.backtrack([v], expand_out, reduce_off)
    paths
  end

  def self.two_colorable?(v, edges) # two-colorable? means is_bipartite?
    bipartite = true
    entered, colors = [], []
    enter_v_iff = lambda { |v| entered[v] = true if bipartite && !entered[v] }
    cross_e = lambda do |e, x|
      bipartite &&= colors[x] != colors[e.y]
      colors[e.y] = !colors[x] # inverts the color
    end
    edges.each_index do |v|
      if !entered[v]
        entered.clear
        colors.clear
        colors[v] = true
        BFS(v, edges, enter_v_iff, nil, cross_e)
      end
    end
    bipartite
  end

  def self.DFS(v, edges, enter_v_iff = nil, exit_v = nil, cross_e = nil)
    if enter_v_iff.nil? || enter_v_iff.call(v)
      (edges[v] or []).each do |e|
        cross_e and cross_e.call(e, v)
        DFS(e.y, edges, enter_v_iff, exit_v, cross_e)
      end
      exit_v and exit_v.call(v) 
    end
  end

  def self.BFS(v, edges, enter_v_iff = nil, exit_v = nil, cross_e = nil)
    q = []
    q.push(v) # offer
    until q.empty?
      v = q.shift # poll
      if enter_v_iff.nil? || enter_v_iff.call(v)
        (edges[v] or []).each do |e|
          cross_e and cross_e.call(e, v)
          q.push(e.y)
        end
        exit_v and exit_v.call(v)
      end
    end
  end
end

class BinaryHeap # min-heap by default, http://en.wikipedia.org/wiki/Binary_heap
  # http://docs.oracle.com/javase/8/docs/api/java/util/PriorityQueue.html
  # a binary heap is a complete binary tree, where all levels but the last one are fully filled, and
  # each node is smaller than or equal to each of its children according to a comparer specified.
  # In Java, new PriorityQueue<Node>(capacity, (a, b) -> a.compareTo(b));
  def initialize(comparer = lambda { |a, b| a <=> b }, hash = lambda { |e| e.hash }) # min-heap by default
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
      @h.delete(@hash[head])
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

class SimpleBinaryHeap # min-heap by default, http://en.wikipedia.org/wiki/Binary_heap
  # http://docs.oracle.com/javase/7/docs/api/java/util/PriorityQueue.html
  # a binary heap is a complete binary tree, where all levels but the last one are fully filled, and
  # each node is smaller than or equal to each of its children according to a comparer specified.
  # In Java, new PriorityQueue<Node>(capacity, (a, b) -> a.compareTo(b));
  def initialize(comparer = lambda { |a, b| a <=> b }) # min-heap by default
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

class MedianBag
  def initialize
    @min_heap = BinaryHeap.new
    @max_heap = BinaryHeap.new(lambda { |a,b| b <=> a })
  end

  def offer(v)
    if @max_heap.size == @min_heap.size
      if @min_heap.peek.nil? || v <= @min_heap.peek
        @max_heap.offer(v)
      else
        @max_heap.offer(@min_heap.poll)
        @min_heap.offer(v)
      end
    else
      if @max_heap.peek <= v
        @min_heap.offer(v)
      else
        @min_heap.offer(@max_heap.poll)
        @max_heap.offer(v)
      end
    end
    self
  end

  def median
    if @max_heap.size == @min_heap.size
      [@max_heap.peek, @min_heap.peek]
    else
      [@max_heap.peek]
    end
  end

  def to_a
    [@max_heap.to_a, @min_heap.to_a]
  end
end

# LRUCache is comparable to this linked hashmap in Java.
class LRUCache
  def initialize(capacity = 1)
    @capacity = capacity
    @hash = {}
    @head = @tail = nil
  end

  def put(k, v)
    @hash[k] and delete_node(@hash[k])
    push_node(DNode.new([k, v]))
    @hash[k] = @tail
    @hash.delete(shift_node.value[0]) while @hash.size > @capacity
    self
  end

  def get(k)
    if @hash[k]
      delete_node(@hash[k])
      push_node(@hash[k])
      @tail.value[1]
    end
  end

  def delete_node(node)
    if @head != node
      node.prev_.next_ = node.next_
    else
      (@head = @head.next_).prev_ = nil
    end
    if @tail != node
      node.next_.prev_ = node.prev_
    else
      (@tail = @tail.prev_).next_ = nil
    end
    self
  end

  def push_node(node) # push at tail
    node.next_ = nil
    node.prev_ = @tail
    if @tail
      @tail.next_ = node
      @tail = @tail.next_
    else
      @head = @tail = node
    end
    self
  end

  def shift_node # pop at head
    if @head
      head = @head
      if @head.next_
        @head = @head.next_
        @head.prev_ = nil
      else
        @head = @tail = nil
      end
      head
    end
  end

  def to_a() @head.to_a end
  def to_s() @head.to_s end

  private :delete_node, :push_node, :shift_node
end

class DNode
  attr_accessor :value, :prev_, :next_

  def initialize(value, next_ = nil, prev_ = nil)
    @value = value; @prev_ = prev_; @next_ = next_
  end

  def to_a
    @next_ ? [value] + @next_.to_a : [value]
  end

  def to_s
    "#{[value, next_ ? next_.to_s: 'nil'].join(' -> ')}"
  end
end

class Edge
  attr_accessor :y, :weight

  def initialize(y, weight = 1)
    @y = y; @weight = weight
  end

  def to_s() y end
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

class TestCases < Test::Unit::TestCase
  def test_topological_sort
    # graph:       D3 ⇾ H7
    #              ↑
    #    ┌──────── B1 ⇾ F5
    #    ↓         ↑     ↑
    #   J9 ⇽ E4 ⇽ A0 ⇾ C2 ⇾ I8
    #              ↓
    #              G6
    edges = []
    edges[0] = [Edge.new(1), Edge.new(2), Edge.new(4), Edge.new(6)] # 1, 2, 4, and 6
    edges[1] = [Edge.new(3), Edge.new(5), Edge.new(9)] # 3, 5, and 9
    edges[2] = [Edge.new(5), Edge.new(8)] # 5, 8
    edges[3] = [Edge.new(7)] # 7
    edges[4] = [Edge.new(9)] # 9
    edges[5] = edges[6] = edges[7] = edges[8] = edges[9] = []
    assert_equal [7, 3, 5, 9, 1, 8, 2, 4, 6, 0], Graph.topological_sort(edges)
  end

  def test_has_cycle_in_directed_n_undirected_graphs
    # graph: B1 ← C2 → A0
    #        ↓  ↗
    #        D3 ← E4
    edges = []
    edges << [] # out-degree of 0
    edges << [Edge.new(3, 4)] # B1 → D3
    edges << [Edge.new(0, 4), Edge.new(1, 6)] # C2 → A0, C2 → B1
    edges << [Edge.new(2, 9)] # D3 → C2
    edges << [Edge.new(3, 3)] # E4 → D3
    assert Graph.has_cycle?(edges, true)

    
    # graph: B1 ← C2 → A0
    #         ↓    ↓
    #        D3 ← E4
    edges = []
    edges << []
    edges << [Edge.new(3)]
    edges << [Edge.new(0), Edge.new(1), Edge.new(4)]
    edges << []
    edges << [Edge.new(3)]
    assert Graph.has_cycle?(edges, true)

    # undirected graph: A0 - B1 - C2
    edges = []
    edges[0] = [Edge.new(1)] # A0 - B1
    edges[1] = [Edge.new(0), Edge.new(2)] # B1 - A0, B1 - C2
    edges[2] = [Edge.new(1)] # C2 - B1
    assert !Graph.has_cycle?(edges, false)

    # undirected graph: A0 - B1 - C2 - A0
    edges[0] << Edge.new(2) # A0 - C2
    edges[2] << Edge.new(0) # C2 - A0
    assert Graph.has_cycle?(edges, false)
  end

  def test_4_2_reachable?
    # Given a directed graph, design an algorithm to find out whether there is a route between two nodes.
    # http://iamsoftwareengineer.blogspot.com/2012/06/given-directed-graph-design-algorithm.html
    # graph: B1 ← C2 → A0
    #        ↓  ↗
    #        D3 ← E4
    edges = []
    edges << [] # out-degree of 0
    edges << [Edge.new(3)] # B1 → D3
    edges << [Edge.new(0), Edge.new(1)] # C2 → A0, C2 → B1
    edges << [Edge.new(2)] # D3 → C2
    edges << [Edge.new(3)] # E4 → D3

    can_reach = lambda do |source, sink|
      all = Graph.find_all(source, edges)
      all.index(sink)
    end

    assert can_reach.call(4, 0)
    assert !can_reach.call(0, 4)
    assert !can_reach.call(3, 4)
  end

  def test_binary_heap
    h = BinaryHeap.new(lambda { |a, b| b[1] <=> a[1] }, lambda { |e| e[0] })
    h.offer(['d', 10])
    h.offer(['e', 30])
    h.offer(['h', 50]).
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

  def test_max_flow_ford_fulkerson
@@bipartite = <<HERE
    A0 -⟶ B1 ⟶ D3
       ↘     ↘    ↘
          C2 ⟶ E4 ⟶ F5
HERE
    edges = []
    edges[0] = [Edge.new(1), Edge.new(2)]
    edges[1] = [Edge.new(3), Edge.new(4)]
    edges[2] = [Edge.new(4)]
    edges[3] = [Edge.new(5)]
    edges[4] = [Edge.new(5)]
    edges[5] = []
    capacities = []
    capacities[0] = [0, 1, 1, 0, 0, 0]
    capacities[1] = [0, 0, 0, 1, 1, 0]
    capacities[2] = [0, 0, 0, 0, 1, 0]
    capacities[3] = [0, 0, 0, 0, 0, 1]
    capacities[4] = [0, 0, 0, 0, 0, 1]
    capacities[5] = [0, 0, 0, 0, 0, 0]
    max_flow = Graph.max_flow(0, 5, edges, capacities)
    assert_equal 2, max_flow.reduce(0) { |max, e| max += e[0] }
    assert_equal [[1, "A→C→E→F"], [1, "A→B→D→F"]], max_flow.map { |e| [e[0]] + [e[1].map { |c| ('A'[0] + c).chr }.join('→')] }

@@graph = <<HERE
    A0 ---⟶ D3 ⟶ F5
    │ ↖   ↗ │     │
    │   C2   │     │
    ↓ ↗  ↘  ↓     ↓
    B1 ⟵--- E4 ⟶ G6
HERE

      edges = []
      edges[0] = [Edge.new(1), Edge.new(3)]
      edges[1] = [Edge.new(2)] # B1 → C2
      edges[2] = [Edge.new(0), Edge.new(3), Edge.new(4)] # C2 → A0, D3, D4
      edges[3] = [Edge.new(4), Edge.new(5)] # D3 → E4, F5
      edges[4] = [Edge.new(1), Edge.new(6)] # E4 → B1, G6
      edges[5] = [Edge.new(6)]
      edges[6] = []
      capacities = []
      capacities[0] = [0, 3, 0, 3, 0, 0, 0]
      capacities[1] = [0, 0, 4, 0, 0, 0, 0]
      capacities[2] = [3, 0, 0, 1, 2, 0, 0]
      capacities[3] = [0, 0, 0, 0, 2, 6, 0]
      capacities[4] = [0, 1, 0, 0, 0, 0, 1]
      capacities[5] = [0, 0, 0, 0, 0, 0, 9]
      capacities[6] = [0, 0, 0, 0, 0, 0, 0]
      max_flow = Graph.max_flow(0, 6, edges, capacities)
      assert_equal 5, max_flow.reduce(0) { |max, e| max += e[0] }
      assert_equal [[3, "A→D→F→G"], [1, "A→B→C→D→F→G"], [1, "A→B→C→E→G"]], max_flow.map { |e| [e[0]] + [e[1].map { |c| ('A'[0] + c).chr }.join('→')] }
  end

  def test_navigatable_n_two_colorable
    # Given a undirected graph based on a set of nodes and links, 
    # write a program that shows all the possible paths from a source node to a destination node.
    # It is up to you to decide what kind of structure you want to use to represent the nodes and links.
    # A path may traverse any link at most once.
    #
    # e.g.  a --- d
    #       |  X  |
    #       b --- c
    edges = [] # a composition of a graph
    edges[0] = [Edge.new(1), Edge.new(2), Edge.new(3)]
    edges[1] = [Edge.new(0), Edge.new(2), Edge.new(3)]
    edges[2] = [Edge.new(0), Edge.new(1), Edge.new(3)]
    edges[3] = [Edge.new(0), Edge.new(1), Edge.new(2)]
    paths = Graph.navigate(0, 3, edges)
#    assert_equal [[0, 1, 2, 3], [0, 1, 3], [0, 2, 3], [0, 3]], paths
#    assert_equal ["a→b→c→d", "a→b→d", "a→c→d", "a→d"], paths.map {|a| a.map { |e| ('a'[0] + e).chr }.join('→') }

    # graph: B1 ― A0
    #        |    |
    #        C2 ― D3
    edges = []
    edges << [Edge.new(1), Edge.new(3)] # A0 - B1, A0 - D3
    edges << [Edge.new(0), Edge.new(2)] # B1 - A0, B1 - C2
    edges << [Edge.new(1), Edge.new(3)] # C2 - B1, C2 - D3
    edges << [Edge.new(0), Edge.new(2)] # D3 - A0, D3 - C2
    assert Graph.two_colorable?(0, edges)

    # graph: B1 ― A0
    #        |  X
    #        C2 ― D3
    edges = []
    edges << [Edge.new(1), Edge.new(2)] # A0 - B1, A0 - C2
    edges << [Edge.new(0), Edge.new(2), Edge.new(3)] # B1 - A0, B1 - C2, B1 - D3
    edges << [Edge.new(0), Edge.new(1), Edge.new(3)] # C2 - A0, C2 - B1, C2 - D3
    edges << [Edge.new(1), Edge.new(2)] # D3 - B1, D3 - C2
    assert !Graph.two_colorable?(0, edges)
  end

  def test_graph_coloring
    # http://www.youtube.com/watch?v=Cl3A_9hokjU
    graph = []
    graph[0] = [0, 1, 0, 1]
    graph[1] = [1, 0, 1, 1]
    graph[2] = [0, 1, 0, 1]
    graph[3] = [1, 1, 1, 0]
    assert_equal [3, [0, 1, 0, 2]], Graph.color_vertex(graph)

    graph = []
    graph[0] = [0, 1, 1, 0, 1]
    graph[1] = [1, 0, 1, 0, 1]
    graph[2] = [1, 1, 0, 1, 0]
    graph[3] = [0, 0, 1, 0, 1]
    graph[4] = [1, 1, 0, 1, 0]
    assert_equal [3, [0, 1, 2, 0, 2]], Graph.color_vertex(graph)
  end

  def test_20_9_median
    bag = MedianBag.new
    bag.offer(30).offer(50).offer(70)
    assert_equal [50], bag.median
    assert_equal [30, 50], bag.offer(10).median
    assert_equal [30], bag.offer(20).median
    assert_equal [30, 50], bag.offer(80).median
    assert_equal [50], bag.offer(90).median
    assert_equal [50, 60], bag.offer(60).median
    assert_equal [60], bag.offer(100).median
  end

  def test_LRU_cache
    c = LRUCache.new(3).put(1, 'a').put(2, 'b').put(3, 'c')
    assert_equal 'a', c.get(1)
    assert_equal [[2, "b"], [3, "c"], [1, "a"]], c.to_a
    assert_equal 'b', c.get(2)
    assert_equal [[3, "c"], [1, "a"], [2, "b"]], c.to_a
    assert_equal [[1, "a"], [2, "b"], [4, "d"]], c.put(4, 'd').to_a
    assert_equal nil, c.get(3)
    assert_equal 'a', c.get(1)
    assert_equal [[2, "b"], [4, "d"], [1, "a"]], c.to_a
  end
end