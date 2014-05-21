#!/usr/bin/env ruby

%w{test/unit stringio set}.each { |e| require e }

module Partitions
  def self.int_partition(n) # http://en.wikipedia.org/wiki/Partition_(number_theory)
    # e.g., the seven distinct integer partitions of 5 are 5, 4+1, 3+2, 3+1+1, 2+2+1, 2+1+1+1, and 1+1+1+1+1.
    case
    when 0 == n then []
    when 1 == n then [[1]]
    else
      int_partition(n-1).reduce([]) do |a, p| # partition 'p'
        a << p[0..-2] + [p[-1]+1] if p[-2].nil? || p[-2] > p[-1]
        a << p + [1] # evaluates to self.
      end
    end
  end

  def self.int_composition(n) # http://en.wikipedia.org/wiki/Composition_(number_theory)
    memos = {}
    map = lambda do |n, k|
      memos[n] ||= {}
      memos[n][k] ||= case
      when 0 == k then []
      when 1 == k then [[n]]
      else
        (1...n).reduce([]) { |a, e| a += map.call(n-e, k-1).map { |c| [e] + c } }
      end
    end
    (1..n).reduce([]) { |a, k| a += map.call(n, k) }
  end

  def self.set_partition(ary = [])
    # http://oeis.org/wiki/User:Peter_Luschny/SetPartitions
    prefix_maximums = Array.new(ary.size, 0)
    restricted_keys = Array.new(ary.size, 0)
    partitions = []
    while succ!(restricted_keys, prefix_maximums)
      partitions << ary.each_index.reduce([]) do |p, i|
        (p[restricted_keys[i]] ||= []) << ary[i]; p
      end
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

module Strings
  def self.min_window(positions) # e.g. [[0, 89, 130], [95, 123, 177, 199], [70, 105, 117]], O(L*logK)
    min_window = window = positions.map { |e| e.shift } # [0, 95, 70]
    heap = BinaryHeap.new(lambda { |a, b| a[1] <=> b[1] })
    heap = window.each_index.reduce(heap) { |h, i| h.offer([i, window[i]]) }
    until positions[i = heap.poll[0]].empty?
      window[i] = positions[i].shift
      min_window = [min_window, window].min_by { |w| w.max - w.min }
      heap.offer([i, window[i]])
    end
    min_window.minmax
  end

  def self.min_window_string(text, pattern) # O(|text| + |pattern| + |indices| * log(|pattern|))
    p = pattern.each_char.reduce({}) { |h, e| h.merge(e => 1+(h[e]||0)) } # O(|pattern|)
    t = text.size.times.reduce({}) { |h, i| (h[text[i, 1]] ||= []) << i if p[text[i, 1]]; h } # O(|text|)
    heap = BinaryHeap.new(lambda { |a, b| a[1] <=> b[1] }) # comparator on found indices.
    heap = p.reduce(heap) { |h, (k, v)| v.times { h.offer([k, t[k].shift]) }; h } # offers key-index pairs.
    min_window = window = heap.to_a.map { |kv| kv[1] }.minmax
    until t[k = heap.poll[0]].empty?
      heap.offer([k, t[k].shift]) # offers a key-index pair for the min-index key.
      window = heap.to_a.map { |kv| kv[1] }.minmax
      min_window = [min_window, window].min_by { |w| w.last - w.first }
    end
    text[min_window.first..min_window.last]
  end

  def self.index_of_by_rabin_karp(t, p)
    n = t.size
    m = p.size
    hash_p = hash(p, 0, m)
    hash_t = hash(t, 0, m)
    return 0 if hash_p == hash_t

    a_to_m = 31 ** m
    (0...n-m).each do |offset|
      hash_t = hash_succ(t, offset, m, a_to_m, hash_t)
      return offset+1 if hash_p == hash_t
    end

    -1
  end

  def self.hash_succ(chars, offset, m, a_to_m, hash)
    hash = (hash << 5) - hash
    hash - a_to_m * chars[offset] + chars[offset+m]
  end

  def self.hash(chars, offset, length)
    (offset...offset+length).reduce(0) { |h, i| ((h << 5) - h) + chars[i] }
  end

  def self.regex_match?(text, pattern, i = text.size-1, j = pattern.size-1)
    case
    when j == -1
      i == -1
    when i == -1
      j == -1 || (j == 1 && pattern[1].chr == '*')
    when pattern[j].chr == '*'
      regex_match?(text, pattern, i, j-2) ||
      (pattern[j-1].chr == '.' || pattern[j-1] == text[i]) && regex_match?(text, pattern, i-1, j)
    when pattern[j].chr == '.' || pattern[j] == text[i]
      regex_match?(text, pattern, i-1, j-1)
    else
      false
    end
  end

  def self.wildcard_match?(text, pattern)
    case
    when pattern == '*'
      true
    when text.empty? || pattern.empty?
      text.empty? && pattern.empty?
    when pattern[0].chr == '*'
      wildcard_match?(text, pattern[1..-1]) || 
      wildcard_match?(text[1..-1], pattern)
    when pattern[0].chr == '?' || pattern[0] == text[0]
      wildcard_match?(text[1..-1], pattern[1..-1])
    else
      false
    end
  end

  def self.bracket_match?(s)
    a = []
    s.each_char do |c|
      case c
        when '(' then a.push(')')
        when '[' then a.push(']')
        when '{' then a.push('}')
        when ')', ']', '}'
          return false if a.empty? || c != a.pop
        end
    end
    a.empty?
  end

  def self.combine_parentheses(o, c, s = '')
    case
    when o == 0 && c == 0
      [s]
    when o == c
      combine_parentheses(o-1, c, s + '(')
    when o == 0
      combine_parentheses(o, c-1, s + ')')
    else
      combine_parentheses(o-1, c, s + '(') +
      combine_parentheses(o, c-1, s + ')')
    end
  end

  def self.combine_parens(n)
    answers = []
    expand_out = lambda do |a|
      opens = a.last[1]
      case
      when a.size == 2*opens then [['(', 1+opens]] # open
      when n == opens then [[')', opens]] # close
      else [['(', 1+opens], [')', opens]]  # open, or close
      end
    end

    reduce_off = lambda do |a|
      answers << a.map { |e| e[0] }.join if a.size == 2*n
    end

    Search.backtrack([['(', 1]], expand_out, reduce_off)
    answers
  end

  def self.interleave(a, b)
    answers = []
    expand_out = lambda do |s|
      e = s.last
      [ [e[0]+1, e[1]], [e[0], e[1]+1] ].select { |e| e[0] < a.size && e[1] < b.size }
    end

    reduce_off = lambda do |s|
      if s.size-1 == a.size + b.size
        answers << (1...s.size).reduce('') { |z,i|
          z += a[s[i][0], 1] if s[i-1][0] != s[i][0]
          z += b[s[i][1], 1] if s[i-1][1] != s[i][1]
          z
        }
      end
    end

    Search.backtrack([[-1, -1]], expand_out, reduce_off)
    answers
  end

  def self.anagram?(lhs, rhs) # left- and right-hand sides
    return true if lhs.equal?(rhs) # reference-equals
    return false unless lhs.size == rhs.size # value-equals

    counts = lhs.each_char.reduce({}) { |h, c| h.merge(c => 1 + (h[c] || 0)) }

    rhs.each_char do |c|
      return false unless counts.has_key? c
      counts[c] -= 1
    end

    counts.empty? || counts.values.all? { |e| 0 == e }
  end

  def self.non_repeated(s)
    h = s.each_char.reduce({}) { |h, e| h.merge(e => 1 + (h[e] || 0)) }
    h.keys.select { |k| h[k] == 1 }.sort.join
  end

  def self.lcp(strings = []) # LCP: longest common prefix
    strings.reduce { |l, s| k = 0; k += 1 while l[k] == s[k]; l[0...k] }
  end

  def self.sum(a, b)
    c = ''
    zero = '0'.ord
    tens = ones = 0
    [a.size, b.size].max.times do |i|
      ones = tens
      ones += a[i,1].ord - zero if i < a.size
      ones += b[i,1].ord - zero if i < b.size
      tens, ones = ones / 10, ones % 10
      c += (ones + zero).chr
    end
    c += (tens + zero).chr if tens > 0
    c.reverse
  end

  def self.longest_unique_charsequence(s)
    offset, length = 0, 1
    h = {}
    n = s.size
    i = 0
    n.times do |j|
      if h[s[j]]
        offset, length = i, j-i if j-i > length
        h[s[i]], i = false, i+1 until s[i] == s[j]
        i += 1
      else
        h[s[j]] = true
      end
    end
    offset, length = i, n-i if n-i > length
    s[offset, length]
  end

  def self.longest_common_substring(ary) # of k strings
    suffix_tree = ary.each_index.reduce(Trie.new) do |trie, k| 
      (0...ary[k].size).reduce(trie) { |trie, i| trie[ary[k][i..-1]] = k; trie }
    end

    memos = {}
    exit_v = proc do |key, trie|
      h = trie.value ? {trie.value => nil} : {}
      h = trie.children.values.map { |c| memos[c][1] }.reduce(h) { |h, e| h.merge(e) }
      memos[trie] = [key.join, h]
    end

    suffix_tree.dfs(nil, exit_v, []) # to process in postorder.
    commons = memos.values.select { |v| v[1].size == ary.size }.map { |v| v[0] }
    commons.group_by { |e| e.size }.max.last
  end
end

class SNode
  attr_accessor :value, :next_

  def initialize(value, next_ = nil)
    if value.is_a?(Array)
      value.reverse_each do |v|
        next_ = SNode.new(v, next_)
      end
      @value = next_.value
      @next_ = next_.next_
    else
      @value = value; @next_ = next_
    end
  end

  # Given a list (where k = 7, and n = 5), 1 2 3 4 5 6 7 a b c d e a.
  # 1. do find the length of the loop, i.e., n = 5.
  # 2. begin w/ u = 1, v = 6; advance them k times until they collide.
  def self.find_cycle(head)
    p1 = p2 = head
    while p2 && p2.next_
      p1 = p1.next_
      p2 = p2.next_.next_
      break if p1 == p2
    end
    return if p2.nil?

    n = 1; p1 = p1.next_
    until p1 == p2
      p1 = p1.next_; n += 1
    end

    pk = head; pn_1 = head.next(n-1)
    until pk == pn_1.next
      pk = pk.next; pn_1 = pn_1.next
    end
    pn_1
  end

  def self.sum(lhs, rhs, ones = 0)
    return nil if lhs.nil? && rhs.nil? && 0 == ones
    ones += lhs.value if lhs
    ones += rhs.value if rhs
    SNode.new(ones % 10, sum(lhs ? lhs.next_ : nil, rhs ? rhs.next_ : nil, ones / 10))
  end

  def self.reverse!(current)
    head = nil
    while current
      save = current.next_
      current.next_ = head
      head = current
      current = save
    end
    head
  end

  def self.reverse_every2!(head)
    if head.nil? || head.next.nil?
      head
    else
      next2 = head.next_.next_
      head.next_.next_, head = head, head.next_
      head.next_.next_ = reverse_every2!(next2)
      head
    end
  end

  def last
    last = self
    last = last.next_ while last.next_
    last
  end

  def next(n = 1)
    next_ = self
    n.times { next_ = next_.next_ }
    next_
  end

  def self.eql?(lhs, rhs)
    return true if lhs.nil? && rhs.nil?
    return false if lhs.nil? || rhs.nil?
    return false if lhs.value != rhs.value
    eql?(lhs.next_, rhs.next_)
  end

  def to_s
    "#{[value, next_.nil? ? '⏚' : next_].join(' → ')}"
  end
end

# LRUCache is comparable to this linked hash-map in Java.
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

class CircularBuffer
  def initialize(capacity)
    @ary = Array.new(capacity+1)
    @head = @tail = 0
  end

  def enq(v)
    raise RuntimeError, "This buffer is full." if full?
    @ary[@tail] = v
    @tail = (@tail+1) % @ary.size
    self
  end

  def deq
    raise RuntimeError, "This buffer is empty." if empty?
    v = @ary[@head]
    @head = (@head+1) % @ary.size
    v
  end

  def empty?() @head == @tail end
  def full?() @head == (@tail+1) % @ary.size end
end

# http://en.wikipedia.org/wiki/Patricia_trie
# https://raw.github.com/derat/trie/master/lib/trie.rb
# https://raw.github.com/dustin/ruby-trie/master/lib/trie.rb
class Trie # constructs in O(n^2) time & O(n^2) space; O(n) time & space if optimized.
  def []=(key, value)
    key = key.split('') if key.is_a?(String)
    if key.empty?
      @value = value
    else
      (@children[key[0]] ||= Trie.new)[key[1..-1]] = value
    end
  end

  def [](key = [])
    key = key.split('') if key.is_a?(String)
    if key.empty?
      @value
    elsif @children[key[0]]
      @children[key[0]][key[1..-1]]
    end
  end

  def of(key)
    key = key.split('') if key.is_a?(String)
    if key.empty?
      self
    elsif @children[key[0]]
      @children[key[0]].of(key[1..-1])
    end
  end

  def values
    @children.values.reduce([@value]) {
      |a, c| c.values.reduce(a) { |a, v| a << v } 
    }.compact
  end

  def dfs(enter_v_iff = nil, exit_v = nil, key = [])
    if enter_v_iff.nil? || enter_v_iff.call(key, self)
      @children.keys.each do |k| 
        @children[k].dfs(enter_v_iff, exit_v, key + [k])
      end
      exit_v and exit_v.call(key, self)
    end
  end

  def initialize
    @value = nil
    @children = {}
  end

  attr_reader :value, :children
end

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

class BNode
  attr_accessor :value, :left, :right, :parent

  def initialize(value = nil, left = nil, right = nil, parent = nil)
    @value = value
    @left = left
    @right = right
    @parent = parent
  end

  def self.max_sum_of_path(node)
    if node
      lsum, lmax_sum = max_sum_of_path(node.left)
      rsum, rmax_sum = max_sum_of_path(node.right)
      sum = node.value + [lsum, rsum, 0].max
      max_sum = [lmax_sum, rmax_sum, sum, node.value + lsum + rsum].compact.max
      [sum, max_sum]
    else
      [0, nil]
    end
  end

  def self.path_of_sum(node, sum, breadcrumbs = [], prefix_sums = [], sum_begins_from = { sum => [0] })
    return [] if node.nil?
    paths = []
    breadcrumbs << node.value
    prefix_sums << node.value + (prefix_sums[-1] || 0)
    (sum_begins_from[prefix_sums[-1] + sum] ||= []) << breadcrumbs.size
    (sum_begins_from[prefix_sums[-1]] || []).each do |from|
      paths += [breadcrumbs[from..-1].join(' -> ')]
    end
    paths += path_of_sum(node.left, sum, breadcrumbs, prefix_sums, sum_begins_from)
    paths += path_of_sum(node.right, sum, breadcrumbs, prefix_sums, sum_begins_from)
    sum_begins_from[prefix_sums[-1] + sum].pop
    prefix_sums.pop
    breadcrumbs.pop
    paths
  end

  def self.common_ancestors(root, p, q)
    found = 0
    breadcrumbs = [] # contains ancestors.
    enter = lambda do |v|
      if found < 2 # does not enter if 2 is found.
        breadcrumbs << v if 0 == found
        found += [p, q].count { |e| v.value == e }
        true
      end
    end

    exit = lambda do |v|
      breadcrumbs.pop if found < 2 && breadcrumbs[-1] == v
    end

    dfs(root, enter, exit) # same as follows: order(root, nil, enter, exit)
    breadcrumbs
  end

  def self.succ(node)
    case
    when node.nil? then raise ArgumentError, "'node' must be non-null."
    when node.right then smallest(node.right)
    else
      while node.parent
        break node.parent if node == node.parent.left
        node = node.parent
      end
    end
  end

  def self.smallest(node)
    node.left ? smallest(node.left) : node
  end

  def self.insert_in_order(tree, value)
    if tree.value < value
      if tree.right
        insert_in_order(tree.right, value) 
      else
        tree.right = BNode.new(value)
      end
    else
      if tree.left
        insert_in_order(tree.left, value) 
      else
        tree.left = BNode.new(value)
      end
    end
  end

  def self.last(v, k, a = [k]) # solves the k-th largest element.
    if v
      (a[0] > 0 ? last(v.right, k, a) : []) +
      (a[0] > 0 ? [v.value] : []) +
      ((a[0] -= 1) > 0 ? last(v.left, k, a) : [])
    else
      []
    end
  end

  def self.last2(v, k) # solves the k-th largest element.
    a = []
    reverse(v, lambda { |v| a << v.value }, lambda { |v| a.size < k  }, nil)
    a
  end

  def self.reverse(v, process, enter_iff = nil, exit = nil)
    if v && (enter_iff.nil? || enter_iff.call(v))
      reverse(v.right, process, enter_iff, exit)
      process and process.call(v)
      reverse(v.left, process, enter_iff, exit)
      exit and exit.call(v)
    end
  end

  def self.order(v, process, enter_iff = nil, exit = nil)
    if v && (enter_iff.nil? || enter_iff.call(v))
      order(v.left, process, enter_iff, exit)
      process and process.call(v)
      order(v.right, process, enter_iff, exit)
      exit and exit.call(v)
    end
  end

  def self.order_by_stack(v, process)
    stack = []
    while v || !stack.empty?
      if v
        stack.push(v)
        v = v.left
      else
        v = stack.pop
        process.call(v)
        v = v.right
      end
    end
  end

  def self.dfs(v, enter_iff = nil, exit = nil)
    if enter_iff.nil? || enter_iff.call(v)
      [v.left, v.right].compact.each { |w| dfs(w, enter_iff, exit) }
      exit and exit.call(v)
    end
  end

  def self.bfs(v, enter_iff = nil, exit = nil)
    q = []
    q << v # enque, or offer
    until q.empty?
      v = q.shift # deque, or poll
      if enter_iff.nil? || enter_iff.call(v)
        [v.left, v.right].compact.each { |w| q << w }
        exit and exit.call(v)
      end
    end
  end

  def self.maxsum_subtree(v)
    maxsum = 0
    sums = {}
    exit = lambda do |v|
      sums[v] = [v.left, v.right].compact.reduce(v.value) do |sum, e|
        sum += sums[e]; sums.delete(e); sum
      end
      maxsum = [maxsum, sums[v]].max
    end
    dfs(v, nil, exit)
    maxsum
  end

  def self.parse(preorder, inorder, range_in_preorder = 0..preorder.size-1, range_in_inorder = 0..inorder.size-1)
    # http://www.youtube.com/watch?v=PAYG5WEC1Gs&feature=plcp
    if range_in_preorder.count > 0
      v = preorder[range_in_preorder][0..0]
      pivot = inorder[range_in_inorder].index(v)
      n = BNode.new(v)
      n.left  = parse(preorder, inorder, range_in_preorder.begin+1..range_in_preorder.begin+pivot, range_in_inorder.begin..range_in_inorder.begin+pivot-1)
      n.right = parse(preorder, inorder, range_in_preorder.begin+pivot+1..range_in_preorder.end, range_in_inorder.begin+pivot+1..range_in_inorder.end)
      n
    end
  end

  def self.balanced?(tree)
    max_depth(tree) - min_depth(tree) <= 1
  end

  def self.diameter(tree, memos = {})
    if tree
      [
        max_depth(tree.left) + max_depth(tree.right) + 1,
        self.diameter(tree.left, memos),
        self.diameter(tree.right, memos)
      ].max
    else
      0
    end
  end

  def self.min_depth(tree)
    tree ? 1 + [min_depth(tree.left), min_depth(tree.right)].min : 0
  end

  def self.max_depth(tree)
    tree ? 1 + [max_depth(tree.left), max_depth(tree.right)].max : 0
  end

  def self.size(tree)
    tree ? tree.left.size + tree.right.size + 1 : 0
  end

  def self.sorted?(tree)
    sorted = true
    prev_value = nil
    process_v_iff = lambda do |v|
      sorted &&= prev_value.nil? || prev_value <= v.value
      prev_value = v.value
    end
    order(tree, process_v_iff, lambda { |_| sorted }, nil)
    sorted
  end

  def self.sorted_by_minmax?(tree, min = nil, max = nil)
    tree.nil? ||
    (min.nil? || tree.value >= min) &&
    (max.nil? || tree.value <= max) &&
    sorted_by_minmax?(tree.left, min, tree.value) &&
    sorted_by_minmax?(tree.right, tree.value, max)
  end

  def self.parent!(node)
    [node.left, node.right].compact.each do |child|
      child.parent = node
      parent!(child)
    end
  end

  def self.include?(tree, subtree)
    return true if subtree.nil?
    return false if tree.nil?
    return true if start_with?(tree, subtree)
    return include?(tree.left, subtree) ||
           include?(tree.right, subtree)
  end

  def self.start_with?(tree, subtree)
    return true if subtree.nil?
    return false if tree.nil?
    return false if tree.value != subtree.value
    return start_with?(tree.left, subtree.left) &&
           start_with?(tree.right, subtree.right)
  end

  def self.eql?(lhs, rhs)
    return true if lhs.nil? && rhs.nil?
    return false if lhs.nil? || rhs.nil?
    return false if lhs.value != rhs.value
    return eql?(lhs.left, rhs.left) &&
           eql?(lhs.right, rhs.right)
  end

  def self.of(values, lbound = 0, rbound = values.size - 1)
    return nil if lbound > rbound
    pivot = (lbound + rbound) / 2;
    bnode = BNode.new(values[pivot])
    bnode.left = of(values, lbound, pivot - 1)
    bnode.right = of(values, pivot + 1, rbound)
    bnode
  end

  def self.to_doubly_linked_list(v)
    head = pred = nil
    exit = lambda do |v|
      if pred
        pred.right = v
      else
        head = v
      end
      v.left = pred
      v.right = nil
      pred = v
    end
    bfs(v, nil, exit)
    head
  end
end

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

class Edge
  attr_accessor :y, :weight

  def initialize(y, weight = 1)
    @y = y; @weight = weight
  end

  def to_s() y end
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

class DNode
  attr_accessor :value, :prev_, :next_

  def initialize(value, next_ = nil, prev_ = nil)
    @value, @prev_, @next_ = value, prev_, next_
  end

  def to_a
    @next_ ? [value] + @next_.to_a : [value]
  end

  def to_s
    "#{[value, next_ ? next_.to_s: 'nil'].join(' -> ')}"
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

  def self.solve_boggle(d, m, k, n = m.size) # k is a max length of a word.
    words = []
    branch_out = lambda do |a| # branches out from the last position (p)
      p = a[-1]
      [ # branches out in 8 directions
        [p[0], p[1]-1], [p[0], p[1]+1], [p[0]-1, p[1]], [p[0]+1, p[1]],
        [p[0]-1, p[1]-1], [p[0]-1, p[1]+1], [p[0]+1, p[1]-1], [p[0]+1, p[1]+1]
      ].reject { |q| # rejects invalid branches
        q[0] < 0 || q[0] >= n || q[1] < 0 || q[1] >= n || p[-1][n*q[0]+q[1]]
      }.map { |q| # attaches a hash of encountered positions
        q << p[-1].merge(n*q[0]+q[1] => true)
      }
    end
    reduce_off = lambda do |a|
      w = a.map { |e| m[e[0]][e[1]] }.join # joins chars into a word
      words << w if d[w] # collects a dictionary word
      a.size >= k # tells when to stop backtracking
    end
    n.times { |i|
      n.times { |j|
        backtrack([[i, j, {n*i+j => true}]], branch_out, reduce_off)
      }
    }
    words
  end

  # a k-combination of a set S is a subset of k distinct elements of S, and 
  # the # of k-combinations is equals to the binomial coefficient, n! / (k! * (n-k)!).
  def self.combination(ary, k = nil, n = ary.size)
    nCk = []
    expand_out = lambda { |c| [ary[c.size], nil] }
    reduce_off = lambda { |c| nCk << c.compact if c.size == n }
    Search.backtrack([], expand_out, reduce_off)
    (k ? nCk.select { |c| c.size == k } : nCk).uniq
  end

  # a k-permutation of a set S is an ordered sequence of k distinct elements of S, and 
  # the # of k-permutation of n objects is denoted variously nPk, and P(n,k), and its value is given by n! / (n-k)!.
  def self.permutation(ary, n = ary.size)
    nPn = []
    indices = (0...n).to_a
    expand_out = lambda { |p| indices - p }
    reduce_off = lambda { |p| nPn << p.map { |i| ary[i] } if p.size == n }
    Search.backtrack([], expand_out, reduce_off)
    nPn.uniq
  end

  def self.permutate(ary, n = ary.size)
    if 1 == n
      [ ary.dup ]
    else
      h = {}
      (0...n).
        select { |i| h[ary[i]] = true unless h[ary[i]] }.
        map do |i|
          ary[n-1], ary[i] = ary[i], ary[n-1]
          p = permutate(ary, n-1)
          ary[n-1], ary[i] = ary[i], ary[n-1]
          p
        end.reduce(:+)
    end
  end

  def self.permutate_succ(ary, n = ary.size)
    (n-1).downTo(1) do |i|
      if ary[i] > ary[i-1]
        ((n-i)/2).times { |j| ary[i+j], ary[n-1-j] = ary[n-1-j], ary[i+j] }
        (i...n).each { |j| if ary[j] > ary[i-1]; ary[i], ary[j] = ary[j], ary[i]; return end }
      end
    end
    (n/2).times { |j| ary[j], ary[n-1-j] = ary[n-1-j], ary[j] }
  end

  # http://www.youtube.com/watch?v=p4_QnaTIxkQ
  def self.queens_in_peace(n)
    answers = []
    peaceful_at = lambda do |queens, c|
      queens.each_with_index { |e, i| e != c && queens.size - i != (e - c).abs }
    end

    expand_out = lambda do |queens|
      n.times.select { |c| peaceful_at.call(queens, c) }
    end

    reduce_off = lambda do |queens|
      answers << queens.dup if queens.size == n
    end

    Search.backtrack([], expand_out, reduce_off)
    answers
  end
end

module DP # http://basicalgos.blogspot.com/search/label/dynamic%20programming
  def self.optimal_tour(g) # TSP http://www.youtube.com/watch?v=aQB_Y9D5pdw
    memos = Array.new(g.size) { {} }
    map = lambda do |v, s| # optimal tour from v to 0 through all vertices in S.
      h = s.keys.sort.reduce(0) { |h, e| h = 31*h + e }
      memos[v][h] ||= case
      when s.empty?
        0 == g[v][0] ? [nil, Float::MAX] : [0, g[v][0]]
      else
        s.keys.reject { |w| 0 == g[v][w] }.
          map { |w| [w, g[v][w] + map.call(w, s.reject { |k,_| k == w })[1]] }.
          min_by { |e| e[1] }
      end
    end

    reduce = lambda do |v, s|
      h = s.keys.sort.reduce(0) { |h, e| h = 31*h + e }
      if s.empty?
        [memos[v][h][0]]
      else
        w = memos[v][h][0]
        [w] + reduce.call(w, s.reject { |k,_| k == w })
      end
    end

    s = (1...g.size).reduce({}) { |h, k| h.merge(k => k) }
    [map.call(0, s)[1], [0] + reduce.call(0, s)]
  end

  def self.optimal_binary_search_tree(keys, probs)
    memos = []
    map = lambda do |i, j|
    memos[i] ||= {}
      memos[i][j] ||= case
      when i > j
        0
      else
        probs_ij = (i..j).map { |k| probs[k] }.reduce(:+)
        comparisons = (i..j).map { |k|
          map.call(i, k-1) + map.call(k+1, j) + probs_ij
        }.min
      end
    end

    map.call(0, keys.size-1)
  end

  # http://basicalgos.blogspot.com/2012/03/35-matrix-chain-multiplication.html
  # http://www.personal.kent.edu/~rmuhamma/Algorithms/MyAlgorithms/Dynamic/chainMatrixMult.htm
  def self.order_matrix_chain_multiplication(p)
    memos = []
    map = lambda do |i, j|
      memos[i] ||= []
      memos[i][j] ||= if i == j
        [nil, 0] # i.e. [move, cost]
      else
        (i...j).map { |k| # maps a range of k to an enumerable of [move, cost].
          [k, map.call(i, k)[1] + map.call(k+1, j)[1] + p[i] * p[k+1] * p[j+1]]
        }.min_by { |e| e[1] }
      end
    end

    reduce = lambda do |i,j|
      k = memos[i][j][0] # note: k is nil when i == j
      k ? reduce.call(i,k) + reduce.call(k+1, j) + [k] : []
    end

    n = p.size - 1
    [map.call(0, n-1)[1], reduce.call(0, n-1)]
  end

  def self.partition_bookshelf(ary, n)
    # Partition S into k or fewer ranges, to minimize the maximum sum 
    #   over all the ranges, without reordering any of the numbers.
    memos = []
    map = lambda do |m, k|
      memos[m] ||= []
      memos[m][k] ||= case
      when k == 1
        [m, (0...m).map { |j| ary[j] }.reduce(:+)]
      when m == 1
        [1, ary[0]]
      else
        (1...m).map { |i|
          [i, [ map.call(i, k-1)[1], ary[i...m].reduce(:+) ].max ]
        }.min_by { |e| e[1] }
      end
    end

    reduce = lambda do |m, k|
      case
      when k == 1
        [ary[0...m]]
      when m == 1
        [ary[0..0]]
      else
        reduce.call(memos[m][k][0], k-1) + [ary[memos[m][k][0]...m]]
      end
    end

    map.call(ary.size, n)
    reduce.call(ary.size, n)
  end

  def self.subset_of_sum(ary, k, n = ary.size, memos = [])
    memos[n] ||= []
    memos[n][k] ||= case
    when n == 0 then
      []
    else
      s = []
      s += [[ary[n-1]]] if ary[n-1] == k
      s += subset_of_sum(ary, k - ary[n-1], n-1, memos).map { |e| e + [ary[n-1]] } if ary[n-1] <= k
      s += subset_of_sum(ary, k, n-1, memos)
    end
  end

  def self.ordinal_of_sum(m, k, n = m) # sum up to m with k out of n numbers.
    if k == 1
      (1..n).select { |e| e == m }.map { |e| [e] } || []
    elsif n == 0
      []
    else
      s = []
      s += ordinal_of_sum(m-n, k-1, n-1).map { |e| e + [n] } if n < m
      s += ordinal_of_sum(m, k, n-1)
    end
  end


  # Given two strings of size m, n and set of operations replace (R), insert (I) and delete (D) all at equal cost.
  # Find minimum number of edits (operations) required to convert one string into another.
  def self.edit(s, t, whole = true, indel = lambda {1}, match = lambda {|a,b| a == b ? 0 : 1}) # cf. http://en.wikipedia.org/wiki/Levenshtein_distance
    moves = [] # moves with costs
    compute_move = lambda do |i, j|
      moves[i] ||= []
      moves[i][j] ||= case
      when 0 == i then whole ? [j > 0 ? :insert : nil, j] : [nil, 0]
      when 0 == j then [i > 0  ? :delete : nil, i]
      else
        [
          [:match, compute_move.call(i-1, j-1)[-1] + match.call(s[i-1], t[j-1])],
          [:insert, compute_move.call(i, j-1)[-1] + indel.call(t[j-1])],
          [:delete, compute_move.call(i-1, j)[-1] + indel.call(s[i-1])]
        ].min_by { |e| e.last }
      end
    end

    compute_path = lambda do |i, j|
      case moves[i][j][0]
      when :match then compute_path.call(i-1,j-1) + (s[i-1] == t[j-1] ? 'M' : 'S')
      when :insert then compute_path.call(i, j-1) + 'I'
      when :delete then compute_path.call(i-1, j) + 'D'
      else ''
      end
    end

    cost = compute_move.call(s.size, t.size)[-1]
    [cost, compute_path.call(s.size, t.size)]
  end

  # comparable to http://en.wikipedia.org/wiki/Levenshtein_distance
  def self.edit_distance(s, t, whole = true, i = s.size, j = t.size, memos = {}) 
    memos[i] ||= {}
    memos[i][j] ||= case
    when 0 == i then whole ? j : 0
    when 0 == j then i
    else
      [
        edit_distance(s, t, i-1, j, memos) + 1,
        edit_distance(s, t, i, j-1, memos) + 1,
        edit_distance(s, t, i-1, j-1, memos) + (s[i-1] == t[j-1] ? 0 : 1)
      ].min
    end
  end

  # http://www.algorithmist.com/index.php/Longest_Increasing_Subsequence
  # http://en.wikipedia.org/wiki/Longest_increasing_subsequence
  # http://stackoverflow.com/questions/4938833/find-longest-increasing-sequence/4974062#4974062
  # http://wordaligned.org/articles/patience-sort
  def self.longest_increasing_subsequence(ary)
    memos = ary.each_index.reduce([]) do |memos, i|
      j = memos.rindex { |m| m.last <= ary[i] } || -1 
      memos[j+1] = -1 != j ? memos[j] + [ary[i]] : [ary[i]]
      memos
    end
    memos.last # e.g. memos: [[1], [1, 2], [1, 3, 3]]
  end

  def self.longest_increasing_subsequence_v2(ary)
    memos = []
    map = lambda do |i|
      memos[i] ||= (0...i).
        map { |k| v = map.call(k)[1]; [k, 1 + v] }.
        select { |e| ary[e[0]] <= ary[i] }.
        max_by { |e| e[1] } || [nil, 1]
    end

    map.call(ary.size-1)
    k = memos.each_index.to_a.max_by { |k| memos[k][1] }
    answer = []
    while k
      answer.unshift(ary[k])
      k = memos[k][0]
    end
    answer
  end

  # http://en.wikipedia.org/wiki/Longest_common_substring_problem
  def self.longest_common_substring(a, b) # best solved by suffix tree
    n, m = a.size, b.size
    memos = []
    longest = []
    1.upto(n) do |i|
      1.upto(m) do |j|
        if a[i-1] == b[j-1]
          l = memos[i][j] = 1 + (memos[i-1][j-1] || 0)
          s = a[i-l, l]
          longest << s if longest.empty? || l == longest[0].size
          longest.replace([s]) if l > longest[0].size
        end
      end
    end
    longest
  end

  # http://www.algorithmist.com/index.php/Longest_Common_Subsequence
  # http://en.wikipedia.org/wiki/Longest_common_subsequence_problem
  # http://wordaligned.org/articles/longest-common-subsequence
  # http://rosettacode.org/wiki/Longest_common_subsequence#Dynamic_Programming_7
  def self.longest_common_subsequence(s, t)
    memos = []
    map = lambda do |i, j|
      memos[i] ||= []
      memos[i][j] ||= case
      when 0 == i || 0 == j then 0
      when s[i-1] == t[j-1] then 1 + map.call(i-1, j-1)
      else [map.call(i, j-1), map.call(i-1, j)].max
      end
    end

    sequences = []
    reduce = lambda do |i, j|
      (sequences[i] ||= {})[j] ||= case
      when 0 == i || 0 == j then ['']
      when s[i-1] == t[j-1]
        reduce.call(i-1,j-1).product([s[i-1, 1]]).map { |e| e.join }
      when memos[i-1][j] > memos[i][j-1] then reduce.call(i-1, j)
      when memos[i-1][j] < memos[i][j-1] then reduce.call(i, j-1)
      else
        a = reduce.call(i-1, j) + reduce.call(i, j-1)
        a.reduce({}) { |h,k| h.merge(k => nil) }.keys
      end
    end

    map.call(s.size, t.size)
    reduce.call(s.size, t.size) 
  end

  # http://wn.com/programming_interview_longest_palindromic_subsequence_dynamic_programming
  # http://tristan-interview.blogspot.com/2011/11/longest-palindrome-substring-manachers.html
  def self.longest_palindromic_subsequence(s)
    memos = []
    map = lambda do |i, j| # map i, j to longest.
      memos[i] ||= []
      memos[i][j] ||= case
      when j == i
        1
      when j == i+1
        s[i] == s[j] ? 2 : 1
      when s[i] == s[j]
        map.call(i+1, j-1) + 2
      else
        [ map.call(i, j-1), map.call(i+1, j) ].max
      end
    end

    map.call(0, s.size-1)
  end

  def self.minimal_coins(k, denominations, memos = {0 => []})
    memos[k] ||= denominations.
      select { |d| d <= k }.
      map { |d| [d] + minimal_coins(k-d, denominations, memos) }.
      min_by { |coins| coins.size }
  end

  def self.knapsack_unbounded(skus, capacity)
    memos = [] # maximum values by k capacity.
    map = lambda do |w|
      memos[w] ||= if w == 0
        [nil, 0]
      else
        skus.each_index.map { |i|
          w >= skus[i][1] ? [i, skus[i][0] + map.call(w - skus[i][1])[1]] : [nil, 0]
        }.max_by { |e| e[1] }
      end
    end

    reduce = lambda do |w|
      if w == 0 then []
      else
        i = memos[w][0]
        [i] + reduce.call(w-skus[i][1])
      end
    end

    [map.call(capacity)[1], reduce.call(capacity)]
  end

  def self.jump_game2(ary, n = ary.size)
    memos = []
    map = lambda do |i|
      memos[i] ||= case
      when ary[i] >= n-1-i then [n-1-i] # takes at most n-1-i steps.
      when ary[i] == 0 then [0] # the last '0' means it's unreachable.
      else
        (1..ary[i]).
          map { |j| [j] + map.call(i + j) }. # concatenates two arrays.
          min_by { |e| e.size } # finds the minimun-size array.
      end
    end
    map.call(0)
  end

  def self.knapsack01(skus, capacity)
    memos = [] # maximum values by i items, and w capacity.
    map = lambda do |n, w|
      memos[n] ||= []
      memos[n][w] ||= case
      when n == 0 then 0 # for any weight w
      when skus[n-1][1] > w then map.call(n-1, w)
      else
        [
          map.call(n-1, w),
          skus[n-1][0] + map.call(n-1, w - skus[n-1][1])
        ].max
      end
    end

    reduce = lambda do |n, w|
      case
      when n == 0 then []
      when memos[n][w] == memos[n-1][w] 
        reduce.call(n-1, w)
      else
        [n-1] + reduce.call(n-1, w - skus[n-1][1])
      end
    end

    [map.call(skus.size, capacity), reduce.call(skus.size, capacity)]
  end

  def self.cut_rod(prices, lengths, length)
    memos = []
    map = lambda do |l|
      memos[l] ||= if l == 0
        [nil, 0]
      else
        (1..[l, lengths.size].min).map { |k|
          [k, map.call(l - k)[1] + prices[k-1]]
        }.max_by { |e| e[1] }
      end
    end

    reduce = lambda do |l|
      if l == 0 then []
      else
        k = memos[l][0]
        [k] + reduce.call(l-k)
      end
    end

    [map.call(length)[1], reduce.call(length)]
  end

  def self.balanced_partition(ary)
    memos = []
    map = lambda do |n, k|
      memos[n] ||= []
      memos[n][k] ||= case
      when 0 == n then 0
      when ary[n-1] > k
        map.call(n-1, k)
      else
        [
          map.call(n-1, k),
          ary[n-1] + map.call(n-1, k - ary[n-1])
        ].max
      end
    end

    reduce = lambda do |n, k|
      case
      when n == 0 then []
      when memos[n][k] == memos[n-1][k] then reduce.call(n-1, k)
      else
        [ary[n-1]] + reduce.call(n-1, k - ary[n-1])
      end
    end

    map.call(ary.size, ary.reduce(:+)/2)
    reduce.call(ary.size, ary.reduce(:+)/2)
  end

  def self.floyd_warshal(graph)
    d = graph.dup # distance matrix
    n = d.size
    n.times do |k|
      n.times do |i|
        n.times do |j|
          if i != j && d[i][k] && d[k][j]
            via_k = d[i][k] + d[k][j]
            d[i][j] = via_k if d[i][j].nil? || via_k < d[i][j]
          end
        end
      end
    end
    d
  end
end

class MinStack
  attr_reader :minimum

  def initialize
    @stack = []
    @minimum = nil
  end

  def push(element)
    if @minimum.nil? || element <= @minimum
      @stack.push(@minimum)
      @minimum = element
    end
    @stack.push element
  end

  def pop
    element = @stack.pop
    @minimum = @stack.pop if @minimum == element
    element
  end
end

module Numbers # discrete maths and bit twiddling http://graphics.stanford.edu/~seander/bithacks.html
  def self.prime?(n)
    n == 2 || n.odd? && 2.step(Math.sqrt(n).floor, 2).all? { |a| 0 != a % n }
  end

  def self.prime(n, certainty = 5)
    # returns when the probability that the number is prime exceeds 96.875% (1 - 0.5 ** certainty)
    # http://rosettacode.org/wiki/Miller-Rabin_primality_test#Ruby
    if n < 4
      n
    else
      n += 1 if n.even?
      logN = Math.log(n).ceil
      loop do
        break n if certainty.times.all? do 
          a = 2 + rand(n - 3) # i.e. a is in range (2..n-2).
          1 == a ** (n-1) % n # Miller-Rabin primality test
        end
        break nil if n > n + logN*3/2
        n += 2
      end
    end
  end

  def self.abs(i)
    negative1or0 = i >> (0.size * 8 - 1) # 63
    (i + negative1or0) ^ negative1or0
  end

  def self.minmax(a, b)
    negative1or0 = a - b >> (0.size * 8 - 1)
    [ b ^ ((a ^ b) & negative1or0), a ^ ((a ^ b) & negative1or0) ]
  end

  def self.sum(a, b)
    if 0 == b
      a
    else
      units = (a ^ b)
      carry = (a & b) << 1
      sum(units, carry)
    end
  end

  def self.divide(dividend, divisor) # implement division w/o using the divide operator, obviously.
    bit = 1
    while divisor <= dividend
      divisor <<= 1
      bit <<= 1
    end

    quotient = 0
    while bit > 0
      divisor >>= 1
      bit >>= 1
      if dividend >= divisor
        dividend -= divisor
        quotient |= bit
      end
    end
    quotient
  end

  def self.opposite_in_sign?(a, b)
    a ^ b < 0
  end

  def self.power_of_2?(x)
    x > 0 && (0 == x & x - 1)
  end

  def self.knuth_suffle!(ary, n = ary.size)
    ary.each_index do |i|
      j = i + rand(n - i) # to index: i + ary.size - i - 1
      ary[j], ary[i] = ary[i], ary[j]
    end
    ary
  end

  def self.reservoir_samples(io, k = 1)
    samples = []
    count = 0
    until io.eof? do
      count += 1
      if samples.size < k
        samples << io.gets.chomp
      else
        s = rand(count)
        samples[s] = io.gets.chomp if s < k
      end
    end

    samples
  end

  def self.weighted_choice(weights)
    pick = rand(weights.reduce(:+))
    weights.size.times do |i|
      pick -= weights[i]
      return i if pick < 0
    end
  end

  def self.rand7()
    begin
      rand21 = 5 * (rand5 - 1) + rand5
    end until rand21 <= 21
    rand21 % 7 + 1 # 1, 2, ..., 7
  end

  def self.rand5()
    rand(5) + 1
  end

  def self.from_excel_column(s)
    columns = 0; cases = 26
    (s.size - 1).times do
      columns += cases; cases *= 26
    end

    ord_a = 'A'.bytes.to_a[0]
    bytes = s.bytes.to_a
    cases = 1
    -1.downto(-s.size) do |k|
      columns += cases * (bytes[k] - ord_a); cases *= 26
    end

    columns + 1
  end

  def self.to_excel_column(n)
    k = 0; cases = 26
    while n > cases
      n -= cases; cases *= 26; k += 1
    end

    n -= 1
    s = ''
    ord_a = 'A'.bytes.to_a[0]
    (k + 1).times do
      s = (n % 26 + ord_a).chr + s
      n /= 26
    end

    s
  end

  def self.reverse_decimal(n)
    reversed = 0
    while n > 0
      reversed *= 10
      reversed += n % 10
      n /= 10
    end
    reversed
  end

  def self.fibonacci(k, memos = [0, 1]) # F0 = 0, F1 = 1, ...
    memos[k] ||= fibonacci(k - 1, memos) + fibonacci(k - 2, memos) if k >= 0
  end
end

class Queueable
  def initialize
    @stack1 = []
    @stack2 = []
  end

  def offer(element)
    @stack1.push(element)
    self
  end

  def poll
    @stack2.push(@stack1.pop) until @stack1.empty? if @stack2.empty?
    @stack2.pop
  end
end


module Arrays
  def self.max_area_in_histogram(heights)
    l = []
    max_area = 0
    area = lambda do |r| # exclusive right end
      h, w = heights[l.pop], r - (l.empty? ? 0 : l.last+1)
      h * w
    end
    heights.each_index do |r|
      until l.empty? || heights[r] > heights[l.last]
        max_area = [max_area, area.call(r)].max
      end
      l.push(r)
    end
    max_area = [max_area, area.call(heights.size)].max until l.empty?
    max_area
  end

  def self.three_sum_closest(ary, q = 0, n = ary.size)
    ary = ary.sort
    tuples = []
    min_diff = nil
    n.times do |i| # 0...n
      pivot, l, r = ary[i], i+1, n-1
      while l < r
        tuple = [pivot, ary[l], ary[r]]
        diff = (q - tuple.reduce(:+)).abs
        case
        when min_diff.nil? || diff < min_diff
          tuples = [tuple]; min_diff = diff
        when diff == min_diff
          tuples << tuple
        end
        case q <=> tuple.reduce(:+)
        when -1 then l += 1
        when  1 then r -= 1
        else l += 1; r -= 1
        end
      end
    end
    tuples
  end

  def self.three_sum(ary, q = 0, n = ary.size) # q is the target number.
    ary = ary.sort
    tuples = []
    n.times do |i| # 0...n
      pivot, l, r = ary[i], i+1, n-1
      while l < r
        tuple = [pivot, ary[l], ary[r]]
        case q <=> tuple.reduce(:+)
        when -1 then l += 1
        when  1 then r -= 1
        else l += 1; r -= 1; tuples << tuple
        end
      end
    end
    tuples
  end

  # http://discuss.leetcode.com/questions/1070/longest-consecutive-sequence
  def self.longest_ranges(ary)
    h = ary.reduce({}) do |h, e|
      ub = e + h[e+1].to_i # upper bound
      lb = e - h[e-1].to_i # lower bound
      h[lb] = h[ub] = ub - lb + 1
      h
    end
    h.group_by { |_,v| v }.max_by { |k,v| k }[1].map { |e| e[0] }.sort
  end

  # http://en.wikipedia.org/wiki/In-place_matrix_transposition
  # http://stackoverflow.com/questions/9227747/in-place-transposition-of-a-matrix
  def self.transpose_to_v1(a, n_c) # to n columns
    case a.size
    when n_c * n_c # square
      n = n_c
      for r in 0..n-2
        for c in r+1..n-1
          v = r*n + c; w = c*n + r
          a[v], a[w] = a[w], a[v]
        end
      end
    else
      transpose = lambda do |i, m_n, n_c| 
        i * n_c % (m_n-1) # mod by m x n - 1.
      end

      h = [] # keeps track of what elements are already transposed.
      m_n = a.size
      (1...m_n-1).each do |i|
        unless h[i]
          j = i
          until i == (j = transpose.call(i, m_n, n_c))
            a[j], a[i] = a[i], a[j] # swaps elements by parallel assignment.
            h[j] = true
          end
        end
      end
    end
    a
  end

  def self.transpose_to(a, n_c) # to n columns
    transpose = lambda do |i, m_n, n| 
      i * n_c % (m_n-1) # mod by m x n - 1.
    end

    tranposed_yet = lambda do |i, m_n, n_c|
      min = i # i must be minimum.
      loop do
        break i == min if min >= (i = transpose.call(i, m_n, n_c))
      end
    end

    m_n = a.size # a.size equals to m x n.
    transposed = 2 # a[0], and a[m_n-1] are transposed.
    (1..m_n-2).each do |i|
      if 1 == i || (transposed < m_n && tranposed_yet.call(i, m_n, n_c))
        j = i
        until i == (j = transpose.call(j, m_n, n_c))
          a[j], a[i] = a[i], a[j] # swaps elements by parallel assignment.
          transposed += 1
        end
        transposed += 1
      end
    end
    a
  end

  def self.min_out_of_cycle(ary, left = 0, right = ary.size - 1)
    # also called the smallest from a rotated list of sorted numbers.
    if right == left
      ary[left]
    else
      pivot = (left + right)/2
      if ary[right] < ary[pivot]
        min_out_of_cycle(ary, pivot + 1, right)
      else
        min_out_of_cycle(ary, left, pivot)
      end
    end
  end

  def self.index_out_of_cycle(ary, key, left = 0, right = ary.size-1)
    while left <= right
      pivot = (left + right) / 2
      if key == ary[pivot]
        return pivot
      else
        if ary[left] <= ary[pivot]
          if ary[pivot] < key || key < ary[left]
            left = pivot + 1
          else
            right = pivot - 1
          end
        else
          if key < ary[pivot] || key > ary[left]
            right = pivot - 1
          else
            left = pivot + 1
          end
        end
      end
    end
  end

  def self.find_occurences(ary, key)
    first_index = first_index(ary, key)
    if first_index
      first_index .. last_index(ary, key, first_index...ary.size)
    end
  end

  def self.first_index(ary, key, range = 0...ary.size)
    if range.count > 1
      pivot = range.minmax.reduce(:+) / 2
      case key <=> ary[pivot]
      when -1 then first_index(ary, key, range.min..pivot-1)
      when 1 then first_index(ary, key, pivot+1..range.max)
      else first_index(ary, key, range.min..pivot) # up to pivot index
      end
    else
      range.min if key == ary[range.min] # nil otherwise
    end
  end

  def self.last_index(ary, key, range = 0...ary.size)
    if range.count > 1
      pivot = (1 + range.minmax.reduce(:+)) / 2
      case key <=> ary[pivot]
      when -1 then last_index(ary, key, range.min..pivot-1)
      when 1 then last_index(ary, key, pivot+1..range.max)
      else last_index(ary, key, pivot..range.max)
      end
    else
      key == ary[range.min] ? range.min : nil
    end
  end

  def self.pairs_of_sum(ary, sum)
    h = {}
    ary.each do |x|
      if h.has_key?(x)
        h[sum - x] = x
      else
        h[sum - x] = nil unless h.has_key?(sum - x)
      end
    end

    h.each.select { |k, v| v }
  end

  def self.sort_using_stack!(a)
    s = []
    until a.empty?
      e = a.pop
      a.push(s.pop) until s.empty? || s.last < e
      s.push(e)
    end

    a.replace(s) # returns s
  end

  def self.indexes_out_of_matrix(m, x)
    row = 0
    col = m[0].size - 1
    while row < m.size && col >= 0
      return [row, col] if x == m[row][col]
      if (m[row][col] > x)
        col -= 1
      else
        row += 1
      end
    end

    [-1, -1]
  end

  def self.merge_sort!(ary = [], range = 0...ary.size, tmp = Array.new(ary.size))
    if range.max - range.min > 0
      pivot = (range.min + range.max) / 2
      merge_sort!(ary, range.min..pivot, tmp)
      merge_sort!(ary, pivot+1..range.max, tmp)
      merge!(ary, range.min, pivot+1, range.max, tmp)
    end
  end

  def self.merge!(ary, left, left2, right2, tmp)
    left1 = left
    right1 = left2 - 1
    last = 0
    while left1 <= right1 && left2 <= right2
      if ary[left1] < ary[left2]
        tmp[last] = ary[left1]; last += 1; left1 += 1
      else
        tmp[last] = ary[left2]; last += 1; left2 += 1
      end
    end

    while left1 <= right1 do tmp[last] = ary[left1]; last += 1; left1 += 1 end
    while left2 <= right2 do tmp[last] = ary[left2]; last += 1; left2 += 1 end
    ary[left..right2] = tmp[0..last-1]
  end

  def self.max_profit(ary) # from a list of stock prices.
    left = 0; max_left = max_right = -1; max_profit = 0
    ary.each_index do |right|
      if ary[right] < ary[left]
        left = right
      elsif (profit = ary[right] - ary[left]) >= max_profit
        max_left = left; max_right = right; max_profit = profit
      end
    end

    [max_profit, [max_left, max_right]]
  end

  def self.maxsum_subarray(a)
    # Kadane's algorithm http://en.wikipedia.org/wiki/Maximum_subarray_problem
    left = 0; max_left = max_right = -1; sum = max_sum = 0
    a.size.times do |right|
      if sum > 0
        sum = sum + a[right]
      else
        left = right; sum = a[right]
      end
      if sum >= max_sum
        max_left = left; max_right = right; max_sum = sum
      end
    end
    [max_sum, [max_left, max_right]]
  end

  def self.maxsum_submatrix(m)
    prefix_sums_v = Array.new(m.size) { Array.new(m[0].size, 0) } # vertically
    m.size.times do |r|
      m[0].size.times do |c|
        prefix_sums_v[r][c] = (r > 0 ? prefix_sums_v[r - 1][c] : 0) + m[r][c]
      end
    end

    max_top = 0, max_left = 0, max_bottom = 0, max_right = 0; max_sum = m[0][0];
    m.size.times do |top| # O (n*(n+1)/2) * O(m) for n * m matrix
      (top...m.size).each do |bottom|
        sum = 0;
        left = 0;
        m[0].size.times do |right| # O(m) given m columns
          sum_v = prefix_sums_v[bottom][right] - (top > 0 ? prefix_sums_v[top-1][right] : 0)
          if sum > 0
            sum += sum_v
          else
            sum = sum_v; left = right
          end

          if sum >= max_sum
            max_top = top;
            max_bottom = bottom;
            max_left = left;
            max_right = right;
            max_sum = sum;
          end
        end
      end
    end

    [max_sum, [max_top, max_left, max_bottom, max_right]]
  end

  def self.max_size_subsquare(m = [[1]])
    m.size.times do |r|
      raise "row[#{r}].size must be '#{m.size}'." unless m.size == m[r].size
    end

    prefix_sums_v = Array.new(m.size) { [] } # vertically
    prefix_sums_h = Array.new(m.size) { [] } # horizontally
    m.size.times do |r|
      m.size.times do |c|
        prefix_sums_v[r][c] = (r > 0 ? prefix_sums_v[r - 1][c] : 0) + m[r][c]
        prefix_sums_h[r][c] = (c > 0 ? prefix_sums_h[r][c - 1] : 0) + m[r][c]
      end
    end

    max_r = max_c = max_size = -1
    m.size.times do |r|
      m.size.times do |c|
        (m.size - [r, c].max).downto(1) do |size|
          if size > max_size && forms_border?(r, c, size, prefix_sums_v, prefix_sums_h)
            max_r = r; max_c = c; max_size = size
          end
        end
      end
    end

    [max_size, [max_r, max_c]]
  end

  def self.forms_border?(r, c, s, prefix_sums_v, prefix_sums_h)
    s == prefix_sums_h[r][c+s-1] - (c > 0 ? prefix_sums_h[r][c-1] : 0) &&
    s == prefix_sums_v[r+s-1][c] - (r > 0 ? prefix_sums_v[r-1][c] : 0) &&
    s == prefix_sums_h[r+s-1][c+s-1] - (c > 0 ? prefix_sums_h[r+s-1][c-1] : 0) &&
    s == prefix_sums_v[r+s-1][c+s-1] - (r > 0 ? prefix_sums_v[r-1][c+s-1] : 0)
  end

  def self.peak(ary, range = 0...ary.size)
    if range.min # nil otherwise
      k = (range.min + range.max) / 2
      case
      when ary[k-1] < ary[k] && ary[k] > ary[k+1] # at peak
        k
      when ary[k-1] < ary[k] && ary[k] < ary[k+1] # ascending
        peak(ary, k+1..range.max)
      else # descending
        peak(ary, range.min..k-1)
      end
    end
  end

  def self.minmax(ary, range = 0...ary.size)
    if range.min >= range.max
      [ary[range.min], ary[range.max]]
    else
      pivot = [range.min, range.max].reduce(:+) / 2
      mm1 = minmax(ary, range.min..pivot)
      mm2 = minmax(ary, pivot+1..range.max)
      [[mm1[0], mm2[0]].min, [mm1[1], mm2[1]].max]
    end
  end

  def self.exclusive_products(ary) # exclusive products without divisions.
    prefix_products = ary.reduce([]) { |a,e| a + [(a[-1] || 1) * e] }
    postfix_products = ary.reverse_each.reduce([]) { |a,e| [(a[0] || 1) * e] + a }
    (0...ary.size).reduce([]) { |a,i| a + [(i > 0 ? prefix_products[i-1] : 1) * (postfix_products[i+1] || 1)] }
  end

  def self.find_odd(ary)
    ary.group_by { |e| e }.detect { |k, v| v.size % 2 == 1 }[0]
  end

  def self.missing_numbers(ary)
    minmax = ary.minmax # by divide and conquer in O(log N)
    h = ary.reduce({}) { |h,e| h.merge(e => 1 + (h[e] || 0)) }
    (minmax[0]..minmax[1]).select { |i| !h.has_key?(i) }
  end

  def self.find_modes_using_map(ary)
    max_occurence = 0 # it doesn't matter whether we begin w/ 0, or 1.
    occurrences = {}
    ary.reduce([]) do |a,e|
      occurrences[e] = 1 + (occurrences[e] || 0)
      if occurrences[e] > max_occurence
        max_occurence = occurrences[e]
        a = [e]
      elsif occurrences[e] == max_occurence
        a <<= e
      else
        a
      end
    end
  end

  def self.find_modes_using_array(ary)
    if ary
      min = ary.min
      max_hits = 1
      modes = []
      hits_by_number = []
      ary.each do |i|
        hits_by_number[i-min] = 1 + (hits_by_number[i-min] || 0)
        if hits_by_number[i-min] > max_hits
          modes.clear
          max_hits = hits_by_number[i-min]
        end
        modes << i if hits_by_number[i-min] == max_hits
      end
      modes
    end
  end

  def self.move_disk(from, to, which)
    puts "move disk #{which} from #{from} to #{to}"
  end

  def self.move_tower(from, to, spare, n)
    if 1 == n
      move_disk(from, to, 1)
    else
      move_tower(from, spare, to, n-1)
      move_disk(from, to, n)
      move_tower(spare, to, from, n-1)
    end
  end
end # end of Arrays

class TestCases < Test::Unit::TestCase
  def test_sum_two_strings
    a = '12345'
    b = '123456789'
    assert_equal '123469134', Strings.sum(a, b)
  end

  def test_3_5_queque_by_good_code_coverage # this test case satisfies condition & loop coverage(s). http://en.wikipedia.org/wiki/Code_coverage
    # Implement a queue using two stacks.
    q = Queueable.new         # stack1: [ ], stack2: [ ]
    q.offer 1                   # stack1: [1], stack2: [ ]
    q.offer 2                   # stack1: [1, 2], stack2: [ ]
    assert_equal 1, q.poll     # stack1: [ ], stack2: [2], coverage: true, and 2 iterations
    assert_equal 2, q.poll     # stack1: [ ], stack2: [ ], coverage: false
    q.offer 3                   # stack1: [3], stack2: [ ]
    assert_equal 3, q.poll     # stack1: [ ], stack2: [ ], coverage: true, and 1 iteration
    assert_equal nil, q.poll   # stack1: [ ], stack2: [ ], coverage: true, and 0 iteration
  end

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
    assert_equal [[1, "A→C→E→F"], [1, "A→B→D→F"]], max_flow.map { |e| [e[0]] + [e[1].map { |c| ('A'.ord + c).chr }.join('→')] }

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
      assert_equal [[3, "A→D→F→G"], [1, "A→B→C→D→F→G"], [1, "A→B→C→E→G"]], max_flow.map { |e| [e[0]] + [e[1].map { |c| ('A'.ord + c).chr }.join('→')] }
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

  def test_diameter_of_btree
    # tree input:   a
    #             b
    #          c    f
    #           d     g
    #            e
    tree = BNode.parse('abcdefg', 'cdebfga')
    assert_equal 6, BNode.diameter(tree)
  end

  def test_from_strings
    # preorder: abcdefg
    # inorder:  cdebagf
    # tree:      a
    #         b    f
    #       c     g
    #        d
    #         e
    #
    tree = BNode.parse('abcdefg', 'cdebagf')
    assert_equal 'a', tree.value
    assert_equal 'b', tree.left.value
    assert_equal 'c', tree.left.left.value
    assert_equal 'd', tree.left.left.right.value
    assert_equal 'e', tree.left.left.right.right.value
    assert_equal 'f', tree.right.value
    assert_equal 'g', tree.right.left.value
    assert_equal nil, tree.left.right
    assert_equal nil, tree.left.left.left
    assert_equal nil, tree.left.left.right.left
    assert_equal nil, tree.left.left.right.right.left
    assert_equal nil, tree.left.left.right.right.right
    assert_equal nil, tree.right.right
    assert_equal nil, tree.right.left.left
    assert_equal nil, tree.right.left.right
  end

  def test_4_1_balanced_n_4_5_binary_search_tree?
    # 4.1. Implement a function to check if a binary tree is balanced.
    # 4.5. Implement a function to check if a binary tree is a binary search tree.
    assert BNode.balanced?(BNode.of([1, 3, 4, 7, 2, 5, 6]))
    assert BNode.sorted?(BNode.of([1, 2, 3, 4, 5, 6, 7]))
    assert !BNode.sorted?(BNode.of([1, 2, 3, 4, 8, 6, 7]))
    assert BNode.sorted_by_minmax?(BNode.of([1, 2, 3, 4, 5, 6, 7]))
    assert !BNode.sorted_by_minmax?(BNode.of([1, 2, 3, 4, 8, 6, 7]))
    values = []
    BNode.order_by_stack(BNode.of([1, 2, 3, 4, 8, 6, 7]), lambda {|v| values << v.value})
    assert_equal [1, 2, 3, 4, 8, 6, 7], values
  end

  def test_4_3_to_binary_search_tree
    # Given a sorted (increasing order) array, implement an algorithm to create a binary search tree with minimal height.
    # tree:   4
    #       2    6
    #      1 3  5 7
    expected = BNode.new(4, BNode.new(2, BNode.new(1), BNode.new(3)), BNode.new(6, BNode.new(5), BNode.new(7)))
    assert BNode.eql?(expected, BNode.of([1, 3, 5, 7, 2, 4, 6].sort))
  end

  def test_convert_binary_tree_to_doubly_linked_list
    # http://www.youtube.com/watch?v=WJZtqZJpSlQ
    # http://codesam.blogspot.com/2011/04/convert-binary-tree-to-double-linked.html
    # tree:   1
    #       2    3
    #      4 5  6 7
    tree = BNode.of([4, 2, 5, 1, 6, 3, 7])
    head = read = BNode.to_doubly_linked_list(tree)
    assert_equal nil, head.left
    values = []
    while read
      values << read.value
      read = read.right
    end
    assert_equal [1, 2, 3, 4, 5, 6, 7], values
  end

  def test_4_6_successor_in_order_traversal
    # tree:   f
    #       a
    #         b
    #           e
    #         d
    #       c
    c = BNode.new('c')
    d = BNode.new('d', c, nil)
    e = BNode.new('e', d, nil)
    b = BNode.new('b', nil, e)
    a = BNode.new('a', nil, b)
    f = BNode.new('f', a, nil)
    BNode.parent!(f)

    assert_equal 'c', BNode.succ(b).value
    assert_equal 'f', BNode.succ(e).value

    assert_equal 'b', BNode.succ(a).value
    assert_equal 'd', BNode.succ(c).value
    assert_equal 'e', BNode.succ(d).value
    assert_equal nil, BNode.succ(f)

    assert_equal ["f"], BNode.last(f, 1)
    assert_equal ["f", "e", "d"], BNode.last(f, 3)
    assert_equal ["f", "e", "d", "c", "b", "a"], BNode.last(f, 6)
    assert_equal ["f", "e", "d", "c", "b", "a"], BNode.last(f, 7)

    assert_equal ["f"], BNode.last2(f, 1)
    assert_equal ["f", "e", "d", "c", "b", "a"], BNode.last2(f, 7)
  end

  def test_dfs_in_binary_trees
    # tree:  a
    #         b
    #        c
    #       d e
    d = BNode.new('d')
    e = BNode.new('e')
    c = BNode.new('c', d, e)
    b = BNode.new('b', c, nil)
    a = BNode.new('a', nil, b)

    preorder = []
    postorder = []
    bfs = []
    BNode.dfs(a, lambda { |v| preorder << v.value })
    BNode.dfs(a, nil, lambda { |v| postorder << v.value })
    BNode.bfs(a, lambda { |v| bfs << v.value }, nil)
    assert_equal 'abcde', preorder.join
    assert_equal 'decba', postorder.join
    assert_equal 'abcde', bfs.join
  end

  def test_maxsum_subtree
    # tree:  -2
    #          1
    #        3  -2
    #      -1
    e = BNode.new(-1)
    c = BNode.new(3, e, nil)
    d = BNode.new(-2, nil, nil)
    b = BNode.new(1, c, d)
    a = BNode.new(-2, b, nil)
    assert_equal 2, BNode.maxsum_subtree(a)
  end

  def test_4_7_lowest_common_ancestor_in_linear_time
    # tree    a
    #           b
    #        c
    #      d   e
    d = BNode.new('d')
    e = BNode.new('e')
    c = BNode.new('c', d, e)
    b = BNode.new('b', c, nil)
    a = BNode.new('a', nil, b)
    assert_equal c, BNode.common_ancestors(a, 'd', 'e')[-1]
    assert_equal c, BNode.common_ancestors(a, 'c', 'd')[-1]
    assert_equal c, BNode.common_ancestors(a, 'c', 'e')[-1]
    assert_equal b, BNode.common_ancestors(a, 'b', 'e')[-1]
    assert_equal nil, BNode.common_ancestors(a, 'b', 'x')[-1]
    assert_equal nil, BNode.common_ancestors(a, 'x', 'y')[-1]
  end

  def test_4_8_binary_tree_value_include
    tree = BNode.new('a', nil, BNode.new('b', BNode.new('c', nil, BNode.new('d')), nil))
    assert BNode.include?(tree, nil)
    assert BNode.include?(tree, tree)
    assert !BNode.include?(tree, BNode.new('e'))
    assert !BNode.include?(tree, BNode.new('c', nil, BNode.new('e')))
    assert BNode.include?(tree, BNode.new('b'))
    assert BNode.include?(tree, BNode.new('c'))
    assert BNode.include?(tree, BNode.new('d'))
    assert BNode.include?(tree, tree.right)
    assert BNode.include?(tree, tree.right.left)
    assert BNode.include?(tree, tree.right.left.right)
    assert BNode.include?(tree, BNode.new('a'))
    assert BNode.include?(tree, BNode.new('a', nil, BNode.new('b')))
    assert BNode.include?(tree, BNode.new('a', nil, BNode.new('b', BNode.new('c'), nil)))
  end

  def test_4_9_find_path_of_sum_in_linear_time
    # You are given a binary tree in which each node contains a value.
    # Design an algorithm to print all paths which sum up to that value.
    # Note that it can be any path in the tree - it does not have to start at the root.
    #
    # tree: -1
    #         ↘
    #           3
    #         ↙
    #       -1
    #      ↙ ↘
    #     2    3
    tree = BNode.new(-1, nil, BNode.new(3, BNode.new(-1, BNode.new(2), BNode.new(3)), nil))
    assert_equal ["-1 -> 3", "3 -> -1", "2", "-1 -> 3"], BNode.path_of_sum(tree, 2)

    #        -5
    #     -3     4
    #    2   8
    #  -6
    # 7   9
    tree = BNode.new(-5, BNode.new(-3, BNode.new(2, BNode.new(-6, BNode.new(7), BNode.new(9)), nil), BNode.new(8)), BNode.new(4))
    assert_equal 10, BNode.max_sum_of_path(tree)[1]
    tree = BNode.new(-3, BNode.new(-2, BNode.new(-1), nil), nil)
    assert_equal -1, BNode.max_sum_of_path(tree)[1]
    tree = BNode.new(-1, BNode.new(-2, BNode.new(-3), nil), nil)
    assert_equal -1, BNode.max_sum_of_path(tree)[1]
  end

#  def test_7_7_kth_integer_of_prime_factors_3_5_n_7
#    assert_equal 45, Math.integer_of_prime_factors(10)
#  end

  def test_20_8_find_query_strings
    # Given a string s and an array of smaller strings Q, write a program that searches s for each string in Q.
    # a suffix tree of bananas
    s = 'bananas'
    suffix_tree = (0...s.size).reduce(Trie.new) { |trie, i| trie[s[i..-1]] = i; trie } # a trie of suffixes
    assert_equal s.size.times.to_a, suffix_tree.values.sort

    # all indices of query strings
    q = %w(b ba n na nas s bas)
    indices_of = lambda { |q| (trie = suffix_tree.of(q)) ? trie.values : nil }
    indices = q.reduce([]) { |a, q| a << indices_of[q] }
    assert_equal [[0], [0], [2, 4], [2, 4], [4], [6], nil], indices

    # auto-complete a prefix
    d = %w(the they they their they're them)
    trie = d.reduce(Trie.new) { |t, w| t[w] = w; t }
    assert_equal ["the", "their", "them", "they", "they're"], trie.of("the").values.sort
    assert_equal "they", trie["they"]

    # longest common substring, or palindrome
    assert_equal ["anana"], String.longest_common_substring(['bananas', 'bananas'.reverse])
    assert_equal ["aba", "bab"], String.longest_common_substring(['abab', 'baba']).sort
    assert_equal ["ab"], String.longest_common_substring(['abab', 'baba', 'aabb'])
    assert_equal ["ab"], String.longest_common_substring(['abab', 'baba', 'aabb'])
    assert_equal "abc", String.longest_unique_charsequence('abcabcbb')
  end

  def test_2_5_sum_of_2_single_linked_lists # 524 + 495 = 1019
    # There are two decimal numbers represented by a linked list, where each node contains a single digit.
    # The digits are stored in reverse order, such that the 1's digit is at the head of the list.
    # Write a function that adds the two numbers and returns the sum as a linked list.
    i524 = SNode.new(4, SNode.new(2, SNode.new(5)))
    i495 = SNode.new(5, SNode.new(9, SNode.new(4)))
    assert_equal "9 → 1 → 0 → 1 → ⏚", SNode.sum(i524, i495).to_s
  end

  def test_2_6_find_cycle_n_reverse_every2!
    # Given a linked list with a cycle, implement an algorithm which returns the node at the beginning of the loop.
    l = SNode.new([1, 2, 3, 4, 5, 6, 7, 'a', 'b', 'c', 'd', 'e'])
    e = l.last
    e.next_ = l.next(7)
    assert_equal 'e', SNode.find_cycle(l).value # has a back-link to cut off.
    assert_equal nil, SNode.find_cycle(SNode.new([1, 2, 3]))
  end

  def test_reverse_every2!
    assert SNode.eql?(SNode.new([5, 4, 3, 2, 1]), SNode.reverse!(SNode.new([1, 2, 3, 4, 5])))
    assert_equal "2 → 1 → ⏚", SNode.reverse_every2!(SNode.new([1, 2])).to_s
    assert_equal "2 → 1 → 3 → ⏚", SNode.reverse_every2!(SNode.new([1, 2, 3])).to_s
    assert_equal "2 → 1 → 4 → 3 → ⏚", SNode.reverse_every2!(SNode.new([1, 2, 3, 4])).to_s
    assert_equal "2 → 1 → 4 → 3 → 5 → ⏚", SNode.reverse_every2!(SNode.new([1, 2, 3, 4, 5])).to_s
  end

  def test_8_5_combine_parenthesis
    # Write a program that returns all valid combinations of n-pairs of parentheses that are properly opened and closed.
    # input: 3 (e.g., 3 pairs of parentheses)
    # output: ()()(), ()(()), (())(), ((()))
    assert_equal ["((()))", "(()())", "(())()", "()(())", "()()()"], Strings.combine_parens(3)
    assert_equal ["ab12", "a1b2", "a12b", "1ab2", "1a2b", "12ab"], Strings.interleave('ab', '12')
    assert_equal 20, Strings.interleave('abc', '123').size
  end

  def test_3_2_min_stack
    # Design and implement a stack of integers that has an additional operation 'minimum' besides 'push' and 'pop',
    # that all run in constant time, e.g., push(2), push(3), push(2), push(1), pop, pop, and minimum returns 2.
    stack = MinStack.new
    assert stack.minimum.nil?
    stack.push 2                  # [nil, 2]
    stack.push 3                  # [nil, 2, 3]
    stack.push 2                  # [nil, 2, 3, 2, 2]
    stack.push 1                  # [nil, 2, 3, 2, 2, 2, 1]
    assert_equal 1, stack.minimum
    assert_equal 1, stack.pop     # [nil, 2, 3, 2, 2]
    assert_equal 2, stack.minimum
    assert_equal 2, stack.pop     # [nil, 2, 3]
    assert_equal 2, stack.minimum
    assert_equal 3, stack.pop     # [nil, 2]
    assert_equal 2, stack.minimum
    assert_equal 2, stack.pop     # []
    assert stack.minimum.nil?
  end

  def test_1_3_anagram?
    # Given two strings, write a method to determine if one is a permutation of the other.
    assert Strings.anagram?(nil, nil)
    assert Strings.anagram?("", "")
    assert !Strings.anagram?("", "x")
    assert Strings.anagram?("a", "a")
    assert Strings.anagram?("ab", "ba")
    assert Strings.anagram?("aab", "aba")
    assert Strings.anagram?("aabb", "abab")
    assert !Strings.anagram?("a", "b")
    assert !Strings.anagram?("aa", "ab")
  end
end