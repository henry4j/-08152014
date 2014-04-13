#!/usr/bin/env ruby

%w{test/unit open-uri}.each { |e| require e }

class Edge
  def initialize(y)
    @y = y
  end

  def to_s() y end
  attr_accessor :y
end

class Graph
  def self.topological_sort(edges)
    sort = []
    entered = []
    enter_v_iff = lambda { |v| entered[v] = true if not entered[v] }
    exit_v = lambda { |v| sort << v }
    edges.size.times do |v|
      dfs(v, edges, enter_v_iff, exit_v) unless entered[v]
    end
    sort
  end

  def self.dfs(v, edges, enter_v_iff = nil, exit_v = nil, cross_e = nil)
    if enter_v_iff.nil? || enter_v_iff.call(v)
      (edges[v] or []).each do |e|
        cross_e.call(v, e) if cross_e
        dfs(e.y, edges, enter_v_iff, exit_v, cross_e)
      end
      exit_v.call(v) if exit_v
    end
  end
end

class TestCases < Test::Unit::TestCase
  def test_balances
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/rubyist/balances-testcases/input00.txt'
    open(test_case_uri) do |io|
      n = io.readline.to_i
      wl = [] # weights on left
      wr = [] # weights on right
      bl = Array.new(n) { [] } # balances on left
      br = Array.new(n) { [] } # balances on right
      g = Array.new(n) { [] } # adj list.
      n.times do |i|
        lines = 2.times.map { io.readline.split }
        wl[i], wr[i] = lines[0][0].to_i, lines[1][0].to_i
        bl[i] = lines[0][1..-1].map { |j| j.to_i }
        br[i] = lines[1][1..-1].map { |j| j.to_i }
        bl[i].each { |j| g[j] << Edge.new(i) }
        br[i].each { |j| g[j] << Edge.new(i) }
      end

      sort = Graph.topological_sort(g)

      xl = Array.new(n, 0) # extra weight on left
      xr = Array.new(n, 0) # extra weight on right
      until sort.empty?
        i = sort.pop
        bl[i].each { |j| wl[i] += wl[j] + wr[j] + 10 }
        br[i].each { |j| wr[i] += wl[j] + wr[j] + 10 }
        case
        when wl[i] > wr[i]
          xr[i] = wl[i] - wr[i]
          wr[i] += xr[i]
        when wl[i] < wr[i]
          xl[i] = wr[i] - wl[i]
          wl[i] += xl[i]
        end
      end

      n.times do |i|
        puts "#{i}: #{xl[i]} #{xr[i]}"
      end
    end
  end
end

=begin

Balances

You have a room-full of balances and weights. Each balance weighs ten pounds and is considered perfectly balanced when the sum of weights on its left and right sides are exactly the same. You have placed some weights on some of the balances, and you have placed some of the balances on other balances. Given a description of how the balances are arranged and how much additional weight is on each balance, determine how to add weight to the balances so that they are all perfectly balanced.

There may be more than one way to balance everything, but always choose the way that places additional weight on the lowest balances.

The input file will begin with a single integer, N, specifying how many balances there are.
Balance 0 is specified by lines 1 and 2, balance 1 is specified by lines 3 and 4, etc...
Each pair of lines is formatted as follows:

WL <balances>
WR <balances>

WL and WR indicate the weight added to the left and right sides, respectively. <balances> is a space-delimited list of the other balance that are on that side of this balance. <balances> may contain zero or more elements.

Consider the following input:

4
0 1
0 2
0
0 3
3
0
0
0

Balance 0 has balance 1 on its left side and balance 2 on its right side
Balance 1 has balance 3 on its right side
Balance 2 has three pounds on its left side
Balance 3 has nothing on it

Since balance 3 has nothing on it it is already perfectly balanced, and weighs a total of 10 pounds.
Balance 2 has no other balance on it, so all we need to do is balance it by putting three pounds on its right side. Now it weighs a total of 16 pounds.
Balance 1 has balance three on its right side, which weighs 10 pounds, so we just put 10 pounds on its left side. Balance 1 weighs a total of 30 pounds.
Balance 0 has balance 1 on its left side (30 pounds), and balance 2 on its right side (16 pounds), we can balance it by adding 14 pounds to the right side.

The output should be N lines long, with the nth line listing the weight added to the nth balance, formatted as follows:

<index>: <weight added to left side> <weight added to right side>

So the output for this problem would be:

0: 0 14
1: 10 0
2: 0 3
3: 0 0

=end
