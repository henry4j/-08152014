#!/usr/bin/env ruby

%w{test/unit open-uri}.each { |e| require e }

class TestCases < Test::Unit::TestCase
  def test_binomial_coefficents
    # https://www.interviewstreet.com/challenges/dashboard/#problem/4fe19c4f35a0e
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/binomial-coefficents-testcases/input00.txt'
    open(test_case_uri) do |f|
      n = f.readline.to_i
      np = n.times.map { |i| f.readline.split.map { |s| s.to_i } }

      memos = {}
      binomal_coef = lambda do |n|
        memos[n] ||= case
        when n == 0 then [1]
        when n == 1 then [1, 1]
        else
          a = binomal_coef.call(n-1)
          [1] + (1...a.size).map { |i| a[i-1] + a[i] } + [1]
        end
      end

      np.map { |e| binomal_coef.call(e[0]).count { |v| 0 == v % e[1] } }.each { |v| puts v }
    end
  end

  @challenge = <<HERE
You have N soldiers numbered from 1 to N. Each of your soldiers is either a liar or a truthful person. You have M sets of information about them. The information is of the following form:

Each line contains 3 integers - A, B and C. This means that in the set of soldiers numbered as {A, A+1, A+2, ..., B}, exactly C of them are liars.
There are M lines like the above.

Let L be the total number of your liar soldiers. Since you can't find the exact value of L, you want to find the minimum and maximum value of L.

Input:

The first line of the input contains two integers N and M.
Each of next M lines contains three integers - A, B and C (1 <= Ai <= Bi <= n) and (0 <= Ci <= Bi-Ai). where Ai, B i and C i refers to the values of A, B and C in the ith line respectively
N and M are not more than 101, and it is guaranteed the given informations are satisfiable. You can always find a situation that satisfies the given information .

Output:

Print two integers Lmin and Lmax to the output.

Sample Input

3 2
1 2 1
2 3 1
 
Sample Output
 
1 2
 
Sample Input
 
20 11
3 8 4
1 9 6
1 13 9
5 11 5
4 19 12
8 13 5
4 8 4
7 9 2
10 13 3
7 16 7
14 19 4
 
Sample Output
 
13 14
 
Explanation
 
In the first sample testcase the first line is "3 2", meaning that there are 3 soldiers and we have two sets of information. The first information is that in the set of soldiers {1, 2} one is a liar and the second piece of information is that in the set of soldiers {2,3} again there is one liar. Now there are two possibilities for this scenario: Soldiers number 1 and 3 are liars or soldier number 2 is liar.
So the minimum number of liars is 1 and maximum number of liars is 2. Hence the answer, 1 2.
HERE

end

