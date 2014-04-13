#!/usr/bin/env ruby

# http://code.google.com/codejam/contests.html
# http://codejamdaemon.blogspot.com/2012/02/warm-up-input-and-output-processing.html

%w{test/unit open-uri}.each { |e| require e }

class DP
  def store_credit(prices)
  end
end

class TestCases < Test::Unit::TestCase
  def test_store_credit # http://code.google.com/codejam/contest/351101/dashboard#s=p0
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/codejam-testcases/A-small-practice.in'
    open(test_case_uri) do |io|
     n = Integer(io.readline.chomp)
     n.times do |k|
       credit = Integer(io.readline.chomp)
       items = Integer(io.readline.chomp)
       prices = io.readline.split(' ').map { |e| e.to_i }
       puts "Case ##{1+k}: " + l
       
     end
    end
  end

  def test_reverse_words # http://code.google.com/codejam/contest/351101/dashboard#s=p1
     test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/codejam-testcases/B-small-practice.in'
     open(test_case_uri) do |io|
      n = Integer(io.readline.chomp)
      n.times do |k|
        l = io.readline.chomp
        reverse = proc do |r|
          (0..(r.max-r.min-1)/2).each { |i| l[r.min+i], l[r.max-i] = l[r.max-i], l[r.min+i] }
        end
        reverse[0...l.size]
        i = 0
        while j = l.index(' ', i)
          reverse[i...j]
          i = j+1
        end
        reverse[i...l.size]
        puts "Case ##{1+k}: " + l
      end
     end
  end
end