#!/usr/bin/env ruby

# http://vincentwoo.com/2011/03/05/facebook-puzzle-sophie/

%w{test/unit open-uri}.each { |e| require e }

class Graph
  def self.floyd_warshal(g)
    d = g.dup # distance matrix
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
  def test_find_sophie
    # http://pravin.insanitybegins.com/posts/facebook-puzzle-find-sophie
    # http://tungwaiyip.info/blog/2010/07/22/find_sophie_solution_facebook_programming_puzzle
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/sophie-testcases/input00.txt'
    open(test_case_uri) do |f|
      n = f.readline.to_i # n: number of vertices
      g = [] # adjacency matrix
      v = [] # vertices
      ids = {} # ids keyed by vertices
      p = [] # probabilities
      n.times do |i|
        v[i], p[i] = f.readline.split
        p[i] = Float(p[i])
        ids[v[i]] = i
        g[i] = Array.new(n, nil)
      end

      m = f.readline.to_i # m: number of edges
      m.times do |i|
        x, y, weight = f.readline.split
        x = ids[x]
        y = ids[y]
        g[x][y] = Float(weight)
        g[y][x] = Float(weight)
      end

      d = Graph.floyd_warshal(g) # distance matrix

      expand_out = lambda do |a|
        in_use = []
        a.size.times {|i| in_use[a[i]] = true; }
        d.size.times.select { |i| not in_use[i] }
      end

      ev = [] # expected values
      er = [] # expected routes
      reduce_off = lambda do |a|
        if a.size == d.size
          dps = 0 # distance prefix sum
          ev << 0
          (a.size - 1).times do |i|
            dps = dps + d[a[i]][a[i+1]]
            ev[-1] = ev[-1] + p[a[i+1]] * dps
          end
          er << a.join('→')
        end
      end

      Search.backtrack([0], expand_out, reduce_off)
      assert_equal 6.0, ev.min
    end
  end
end

###########################################################
# https://facebook.interviewstreet.com/recruit/challenges
###########################################################

@challenge = <<HERE

Find Sophie
After a long day of coding, you love to head home and relax with a loved one. Since that whole relationship thing hasn't been working out for you recently, that loved one will have to be your cat, Sophie. Unfortunately you find yourself spending considerable time after you arrive home just trying to find her. Being a perfectionist and unable to let anything suboptimal be a part of your daily life, you decide to devise the most efficient possible method for finding Sophie.

Luckily for you, Sophie is a creature of habit. You know where all of her hiding places are, as well as the probability of her hiding in each one. You also know how long it takes you to walk from hiding place to hiding place. Write a program to determine the minimum expected time it will take to find Sophie in your apartment. It is sufficient to simply visit a location to check if Sophie is hiding there; no time must be spent looking for her at a location. Sophie is hiding when you enter your apartment, and then will not leave that hiding place until you find her. Your program must take the name of an input file as an argument on the command line.

Input Specifications
The input file starts with a single number, m, followed by a newline. m is the number of locations available for Sophie to hide in your apartment. This line is followed by m lines, each containing information for a single location of the form (brackets for clarity):
<location name> <probability>probability is the probability that Sophie is hiding in the location indicated. The sum of all the probabilities is always 1. The contents of these lines are separated by whitespace. Names will only contain alphanumeric characters and underscores ('_'), and there will be no duplicate names. All input is guaranteed to be well-formed. Your starting point is the first location to be listed, and in effect it costs you no time to check if Sophie is there.

The file continues with a single number, c, followed by a newline. c is the number of connections that exist between the various locations. This line is followed by c lines, each of the form:
<location name> <location name> <seconds> The first two entries are the names of locations and seconds is the number of seconds it takes you to walk between the them. Again these lines are whitespace-delimited. Note that the locations are unordered; you can walk between them in either direction and it will take the same amount of time. No duplicate pairs will be included in the input file, and all location names will match one described earlier in the file.

Example input file: 
4
front_door .2
in_cabinet .3
under_bed .4
behind_blinds .1
5
front_door under_bed 5
under_bed behind_blinds 9
front_door behind_blinds 5
front_door in_cabinet 2
in_cabinet behind_blinds 6

Output Specifications
Your output must consist of a single number followed by a newline, printed to standard out. The number is the minimum expected time in seconds it takes to find Sophie, rounded to the nearest hundredth. Make sure that the number printed has exactly two digits after the decimal point (even if they are zeroes). If it is impossible to guarantee that you will find Sophie, print "-1.00" followed by a newline instead.

Example output: 
6.00

HERE
