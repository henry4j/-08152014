#!/usr/bin/env /usr/local/bin/ruby

%w{test/unit open-uri}.each { |e| require e }

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      n, m = io.readline.chomp.split.map { |e| e.to_i }
      lawn = n.times.map { io.readline.chomp.split.map { |e| e.to_i } }
      [tc, lawn]
      # io.readline unless io.eof?
    end
    cases.each { |e| puts status_by_flood_fill(*e) }
  end

  def self.status_by_sweep_line(tc, lawn, n = lawn.size, m = lawn[0].size)
    maximas_h = []
    maximas_v = []
    n.times do |r|
      m.times do |c|
        maximas_h[r] = [maximas_h[r], lawn[r][c]].compact.max
        maximas_v[c] = [maximas_v[c], lawn[r][c]].compact.max
      end
    end
    mowed = Array.new(n) { Array.new(m, 0) }
    n.times do |r|
      m.times do |c|
        mowed[r][c] = [maximas_h[r], maximas_v[c]].min
      end
    end
    "Case ##{tc}: #{lawn == mowed ? 'YES' : 'NO'}"
  end

  def self.status_by_flood_fill(tc, lawn, n = lawn.size, m = lawn[0].size)
    borders = n.times.reduce([]) { |a, r| a << [r, 0] << [r, m-1] }
    borders = 1.upto(m-2).reduce(borders) { |a, c| a << [0, c] << [n-1, c] }
    h = lawn.reduce({}) { |h, a| a.reduce(h) { |h, e| h.merge(e => 1 + (h[e]|| 0)) } }
    keys = h.keys.sort
    keys.each_index do |i|
      keys[i+1] and borders.each do |n|
        flood_fill(lawn, n, keys[i], keys[i+1])
      end
    end
    "Case ##{tc}: #{lawn.all? { |a| a.all? { |e| e == keys[-1] } } ? 'YES' : 'NO'}"
  end

  def self.flood_fill(m, n, s, t) # fills 's' color with 't' color from node 'n'
    q = [n]
    while n = q.shift
      if m[n[0]][n[1]] == s
        w = e = n
        w = [w[0], w[1]-1] while m[w[0]][w[1]-1] == s
        e = [e[0], e[1]+1] while m[e[0]][e[1]+1] == s
        w[1].upto(e[1]) do |c|
          m[n[0]][c] = t
          q << [n[0]-1, c] if m[n[0]-1] && m[n[0]-1][c] == s
          q << [n[0]+1, c] if m[n[0]+1] && m[n[0]+1][c] == s
        end
      end
    end
    m
  end
end

class TestCases < Test::Unit::TestCase
  def test_fill
    m = [
      [1, 1, 1, 1],
      [1, 0, 0, 1],
      [0, 1, 0, 1],
      [1, 1, 1, 1]
    ]
    assert_equal [[1, 1, 1, 1], [1, 2, 2, 1], [0, 1, 2, 1], [1, 1, 1, 1]], CODE_JAM.flood_fill(m.map(&:dup), [1, 1], 0, 2)
    assert_equal [[1, 1, 1, 1], [1, 0, 0, 1], [2, 1, 0, 1], [1, 1, 1, 1]], CODE_JAM.flood_fill(m.map(&:dup), [2, 0], 0, 2)
  end

  def test_main
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/rubyist/lawnmower-testcases/B-small-practice.in'
    open(test_case_uri) { |io| CodeJam.main(io) }
  end
end

# CodeJam.main(STDIN)

=begin

Problem

Alice and Bob have a lawn in front of their house, shaped like an N metre by M metre rectangle. Each year, they try to cut the lawn in some interesting pattern. They used to do their cutting with shears, which was very time-consuming; but now they have a new automatic lawnmower with multiple settings, and they want to try it out.

The new lawnmower has a height setting - you can set it to any height h between 1 and 100 millimetres, and it will cut all the grass higher than h it encounters to height h. You run it by entering the lawn at any part of the edge of the lawn; then the lawnmower goes in a straight line, perpendicular to the edge of the lawn it entered, cutting grass in a swath 1m wide, until it exits the lawn on the other side. The lawnmower's height can be set only when it is not on the lawn.

Alice and Bob have a number of various patterns of grass that they could have on their lawn. For each of those, they want to know whether it's possible to cut the grass into this pattern with their new lawnmower. Each pattern is described by specifying the height of the grass on each 1m x 1m square of the lawn.

The grass is initially 100mm high on the whole lawn.

Input

The first line of the input gives the number of test cases, T. T test cases follow. Each test case begins with a line containing two integers: N and M. Next follow N lines, with the ith line containing M integers ai,j each, the number ai,j describing the desired height of the grass in the jth square of the ith row.

Output

For each test case, output one line containing "Case #x: y", where x is the case number (starting from 1) and y is either the word "YES" if it's possible to get the x-th pattern using the lawnmower, or "NO", if it's impossible (quotes for clarity only).

Limits

1 ≤ T ≤ 100.

Small dataset

1 ≤ N, M ≤ 10.
1 ≤ ai,j ≤ 2.

Large dataset

1 ≤ N, M ≤ 100.
1 ≤ ai,j ≤ 100.

Sample

Input

3
3 3
2 1 2
1 1 1
2 1 2
5 5
2 2 2 2 2
2 1 1 1 2
2 1 2 1 2
2 1 1 1 2
2 2 2 2 2
1 3
1 2 1

Output

Case #1: YES
Case #2: NO
Case #3: YES

=end
