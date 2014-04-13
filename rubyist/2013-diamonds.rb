#!/usr/bin/env /usr/local/bin/ruby

%w{test/unit open-uri}.each { |e| require e }

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      n, *p = io.readline.chomp.split.map { |e| e.to_i }
      [tc, n, p]
    end
    solve(0, 100, [100, 1])
#    cases.each do |c| 
#      puts solve(*c)
#    end
  end

  def self.drop(n, memos)
    
  end

  def self.solve(tc, n, p)
    map = lambda do |k, q, h|
      if k > 0
        l = [q[0]-1, q[1]-1]
        r = [q[0]+1, q[1]-1]
        l2 = h[l[0]] && h[l[0]] >= l[1]
        r2 = h[r[0]] && h[r[0]] >= r[1]
        if l2 && r2 || q[1] == 0
          h2 = h.dup
          h2[q[0]] = q[1]
          if q == p
            1
          else
            if k > 1
              map.call(k - 1, [0, h2[0] + 2], h2)
            else
              0
            end
          end
        elsif l2
          map.call(k, r, h)
        elsif r2
          map.call(k, l, h)
        else
          0.5 * map.call(k, r, h) + 0.5 * map.call(k, l, h)
        end
      else
        0
      end
    end
    "Case ##{tc}: #{map.call(n, [0, 0], {0 => -2}).to_f}"
  end
end

class TestCases < Test::Unit::TestCase
  def test_main
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/rubyist/diamond-testcases/unit-test.in'
    open(test_case_uri) { |io| CodeJam.main(io) }
  end
end

# CodeJam.main(STDIN)
