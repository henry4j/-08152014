#!/usr/bin/env ruby

%w{test/unit open-uri}.each { |e| require e }

module DP
  def self.optimal_tour(q, g) # TSP http://www.youtube.com/watch?v=aQB_Y9D5pdw
    memos = Array.new(g.size) { {} }
    map = lambda do |v, s| # optimal tour from v to 0 through all vertices in S.
      h = s.keys.sort.reduce(0) { |h, e| h = 31*h + e }
      memos[v][h] ||= case
      when s.empty?
        0 == g[v][q].to_i ? [nil, Float::MAX] : [q, g[v][q]]
      else
        s.keys.reject { |w| 0 == g[v][w].to_i }.
          map { |w|
            [w, g[v][w] + map.call(w, s.reject { |k,_| k == w })[1]] 
          }.min_by { |e| e[1] }
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

    s = (0...g.size).reject { |i| g[i].nil? || q == i }.reduce({}) { |h, k| h.merge(k => nil) }
    [map.call(q, s)[1], [q] + reduce.call(q, s)]
  end
end

class TestCases < Test::Unit::TestCase
  def test_solution
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/facebull-testcases/input00.txt'
    g = [] # graph of costs
    m = [] # machines
    open(test_case_uri) do |io|
      until io.eof? do
        l = io.readline.split
        i, j = l[1][1..-1].to_i, l[2][1..-1].to_i
        (g[i] ||= [])[j] = l[3].to_i
        (m[i] ||= [])[j] = l[0][1..-1].to_i
      end
    end

    tour = DP.optimal_tour(g.size-1, g)
    assert_equal [617317, [3, 2, 1, 3]], tour
    puts tour[0] # sum price of all the machines.
    puts tour[1].each_cons(2).map { |e| m[e[0]][e[1]] }.sort.join(' ') # machines numbers in ascending order.
  end
end

=begin

FaceBull

Facebook's Chemical Industry Applications lab has been researching new ways to provide chemical stimulants to keep its engineers productive and awake. Research has isolated a series of chemical compounds when present together in sugar water, are the active ingredients of the next iteration of super-energy drink. 

These chemical compounds are similar, any one compound can be used in a chemical reaction to create any other compound. Although the actual chemical reaction is negligibly cheap to cause, it requires specific expensive machinery (and catalysts) for each reaction. Machinery is specifically tailored to take one compound, and convert it into a different compound. Each machine has a purchase cost, a starting compound, and the final compound it produces. Facebook's lab must acquire enough machinery to ensure it can create all the necessary compounds no matter what compounds are available on the market. 

Write a program that takes a single argument on the command line. This argument must be a file name, which contains the input data. The program should output to standard out the cheapest way Facebook can produce its drink from any source compound or compounds (see below for output details). All chemicals mentioned in the machine catalog are assumed to be necessary for production of the drink. The catalog does not guarantee that any particular one-step reaction is possible, but it is guaranteed to have a way to transform any given compound into any other. Your submission and code solution must follow the input and output specifications below or it will be disqualified. Your program will be tested against several machine catalogs, each with different data. Additionally, your program must be fast, and give a correct answer within a matter of minutes. 


Input specifications

The input file will contain a catalogue of available machinery. Each line of the file represents a particular machine model. The format of each line is:
<machine name>  <orig compound>  <new compound>  <price>
Machine names start with the character 'M' (without the quotes) and then a non-zero padded integer. Examples of machine names are 'M1', 'M2', 'M13', etc. 

Compound names start with the character 'C' (without the quotes) and then a non-zero padded integer. Examples of compound names are 'C4', 'C6', 'C24', etc. 

Prices are simply non-zero padded integers with no commas or periods or unit designations (assume all prices are in USD). Examples of prices are '987334', '13948295', etc. 

Each field in the input file is white space separated using spaces or tab characters. There may or may not be additional white space after the last line of entries. You are guaranteed your program will run against input files that are well formed. 

Example input file:
M1      C1      C2      277317
M2      C2      C1      26247
M3      C1      C3      478726
M4      C3      C1      930382
M5      C2      C3      370287
M6      C3      C2      112344



Output specifications

Your output must be exactly the following. Your program must first output the sum price of all the machines to be purchased (do not insert commas, periods, or any units) as a simple integer, followed by a new line. 

Your program should then output the numbers of the machines purchased in ascending order, but omitting the 'M' designations. Each machine number must be separated by a single space character. There should be no additional space character after the last machine. Instead, the program should output a new line after the last machine number. 

Example output (there should be a newline at the end of each line):
617317
2 3 6

=end
