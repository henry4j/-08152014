#!/usr/bin/env ruby

%w{test/unit open-uri}.each { |e| require e }

class DP
  # http://www8.cs.umu.se/kurser/TDBAfl/VT06/algorithms/BOOK/BOOK2/NODE46.HTM
  # http://en.wikipedia.org/wiki/Levenshtein_distance
  def self.edit_distance(s, t, bound = nil, whole = true, i = s.size, j = t.size, memos = [])
    memos[i] ||= []
    memos[i][j] ||= case
    when 0 == i then whole ? j : 0
    when 0 == j then i
    when bound && (j - i).abs >= bound then bound
    else
      [
        edit_distance(s, t, bound, whole, i-1, j-1, memos) + (s[i-1] == t[j-1] ? 0 : 1),
        edit_distance(s, t, bound, whole, i-1, j, memos) + 1,
        edit_distance(s, t, bound, whole, i, j-1, memos) + 1
      ].min
    end
  end
end

class TestCases < Test::Unit::TestCase
  def test_breathalyzer
    dictionary_uri = '/tmp/twl06.txt' # 178,690 words
    test_case_uri = '/tmp/breathalyzer-input00.txt' # 6 words
    system '/usr/local/bin/wget -O /tmp/twl06.txt https://raw.github.com/henry4j/-/master/algorist/ruby/breathalyzer-testcases/twl06.txt' unless File.exists?(dictionary_uri)
    system '/usr/local/bin/wget -O /tmp/breathalyzer-input00.txt https://raw.github.com/henry4j/-/master/algorist/ruby/breathalyzer-testcases/input00.txt' unless File.exists?(test_case_uri)

    d = {}
    open(dictionary_uri) do |io|
      until io.eof?
        w = io.readline.chomp
        (d[w.length] ||= {})[w] = 0 unless w.empty?
      end
    end

    open(test_case_uri) do |io|
      until io.eof?
        post = io.readline.chomp.upcase.split
        distances = post.map do |s|
          if d[s.length] && d[d.length][s]
            0
          else
            keys = d.keys.sort_by { |l| (l - s.size).abs }
            keys.reduce(Float::MAX) do |min, l|
              d[l].keys.reduce(min) do |min, t|
                if (s.size - t.size).abs < min
                  [min, DP.edit_distance(s, t, min)].min
                else
                  min
                end
              end
            end
          end
        end
        puts distances.reduce(:+)
      end
    end
  end
end


=begin

Breathalyzer

To safeguard against the dreaded phenomenon of wall posting while drunk, Facebook is implementing a feature that detects when post content is too garbled to have been done while sober and informs the user that they need to take an online breathalyzer test before being allowed to post. 

Unfortunately, there is far too much content for a given set of persons to evaluate by hand. Fortunately, you are a programmer of some repute and can write a program that processes and evaluates wall posts. You realize that such a program would be of great use to society and intend to resolve the problem once and for all. The program you write must compute a score for a body of text, returning this score upon completion. 

Your program will be given a list of accepted words and run on one wall post at a time. For each word W in the post, you must find word W' from the list of accepted words such that the number of changes from W to W' is minimized. It is possible that W is already W' and thus the number of changes necessary is zero. A change is defined as replacing a single letter with another letter, adding a letter in any position, or removing a letter from any position. The total score for the wall post is the minimum number of changes necessary to make all words in the post acceptable. 

Input Specification

Your program must take a single string argument, representing the file name containing the wall post to analyze. In addition, your program must open up and read the accepted word list from the following static path location:

/var/tmp/twl06.txt

For testing purposes, you may download and examine the accepted word list here. When submitting your code, you do not need to include this file, as it is already present on the machine. 

The input file consists entirely of lower case letters and space characters. You are guaranteed that the input file will start with a lower case letter, and that all words are separated by at least one space character. The file may or may not end with a new line character. 

Example input file:
tihs sententcnes iss nout varrry goud

You are guaranteed that your program will run against well formed input files and that the accepted word list is identical to the one provided for testing. 

Output Specification

Your program must print out the minimum number of changes necessary to turn all words in the input wall post into accepted words as defined by the word list file. Words may not be joined together, or separated into multiple words. A change in a word is defined as one of the following:
Replacing any single letter with another letter.
Adding a single letter in any position.
Removing any single letter.
This score must be printed out as an integer and followed by a single new line. 

Example Output (newline after number):
8

=end
