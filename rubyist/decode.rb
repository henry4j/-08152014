#!/usr/bin/env ruby

%w{test/unit open-uri}.each { |e| require e }

class TestCases < Test::Unit::TestCase
  def test_decode
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/ruby/decode-testcases/input01.txt'
    open(test_case_uri) do |io|
      io.readline
      words = {} # discards the header '//dict'
      until "//secret" == (w = io.readline.chomp)
        (words[w.size] ||= []).push(w)
      end
      until io.eof?
        old_mappings = [{}] # starts over with new mappings for a new secret.
        s = io.readline.chomp
        s.split.each { |e|
          new_mappings = get_mappings(e, words)
          old_mappings = old_mappings.reduce([]) { |a, old|
            new_mappings.reduce(a) { |a, new|
              a << old.merge(new) if new.all? { |k, v| old[k].nil? || old[k] == v }
              a
            }
          }
        }
        t = s.each_char.map { |c| c == ' ' ? c : old_mappings[0][c] }.join
        puts "#{s} = #{t}"
      end
    end
  end

  def get_mappings(secret, words)
    words[secret.length].map { |w|
      secret.length.times.reduce({}) { |h, i|
        break nil if h[secret[i,1]] && h[secret[i,1]] != w[i,1]
        h.merge(secret[i,1] => w[i,1])
      }
    }.compact
  end
end
