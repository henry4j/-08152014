#!/usr/bin/env ruby

# http://www.kelvinjiang.com/2010/09/facebook-puzzles-and-liar-liar.html

%w{test/unit open-uri}.each { |e| require e }

class Edge
  def initialize(y, weight = 1)
    @y = y
    @weight = weight
  end

  def to_s() y end
  attr_accessor :y, :weight
end

class Graph
  def self.bicolor(edges) # two-colorable? means is_bipartite?
    colors = []
    bipartite = true
    entered = []
    enter_v_iff = lambda do |v|
      entered[v] = true if bipartite and not entered[v]
    end

    cross_e = lambda do |x, e|
      bipartite &&= colors[x] != colors[e.y]
      colors[e.y] = !colors[x]
    end

    edges.each_index do |v|
      (colors[v] = true) and dfs(v, edges, enter_v_iff, nil, cross_e) if not entered[v]
    end

    if bipartite
      colors.each_index.group_by { |i| colors[i] }.values
    end
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
  def test_liar_liar
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/liar-liar-testcases/input00.txt'
    uids = {}
    graph = []
    open(test_case_uri) do |io|
      n = io.readline.chomp.to_i
      n.times do
        u, m = io.readline.split
        uids[u] = uids.size unless uids[u]
        m.to_i.times do
          w = io.readline.chomp
          uids[w] = uids.size unless uids[w]
          (graph[uids[u]] ||= []) << Edge.new(uids[w])
        end
      end

      assert_equal [[0, 1, 3], [2, 4]], Graph.bicolor(graph)
    end
  end
end

=begin

Liar, Liar

As a newbie on a particular internet discussion board, you notice a distinct trend among its veteran members; everyone seems to be either unfailingly honest or compulsively deceptive. You decide to try to identify the members of the two groups, starting with the assumption that every senior member either never lies or never tells the truth. You compile as much data as possible, asking each person for a list of which people are liars. Since the people you are asking have been around on the board for a long time, you may assume that they have perfect knowledge of who is trustworthy and who is not. Each person will respond with a list of people that they accuse of being liars. Everyone on the board can see that you are a tremendous n00b, so they will grudgingly give you only partial lists of who the liars are. Of course these lists are not to be taken at face value because of all the lying going on. 

You must write a program to determine, given all the information you've collected from the discussion board members, which members have the same attitude toward the telling the truth. It's a pretty popular discussion board, so your program will need to be able to process a large amount of data quickly and efficiently. 


Input Specifications

Your program must take a single command line argument; the name of a file. It must then open the file and read out the input data. The data begins with the number of veteran members n followed by a newline. It continues with n chunks of information, each defining the accusations made by a single member. Each chunk is formatted as follows:
 <accuser name> <m>
followed by m lines each containing the name of one member that the accuser says is a liar. accuser name and m are separated by some number of tabs and spaces. m will always be in [0, n]. All member names contain only alphabetic characters and are unique and case-sensitive. 

Example input file:
5
Stephen   1
Tommaso
Tommaso   1
Galileo
Isaac     1
Tommaso
Galileo   1
Tommaso
George    2
Isaac
Stephen

Output Specifications

Your output must consist of two numbers separated by a single space and followed by a newline, printed to standard out. The first number is the size of the larger group between the liars and the non-liars. The second number is the size of the smaller group. You are guaranteed that exactly one correct solution exists for all test data. 

Example output:
3 2

=end
