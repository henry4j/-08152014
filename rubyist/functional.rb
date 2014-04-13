#!/usr/bin/env ruby

# Ruby Styles https://github.com/bbatsov/ruby-style-guide
# First and Rest http://devblog.avdi.org/2010/01/31/first-and-rest-in-ruby/
# http://we4tech.wordpress.com/2012/07/24/four-types-of-ruby-closures-block-proc-lambda-and-method/

class TestCases < Test::Unit::TestCase
  def test_line_of_most_points
    a = [[1, 2], [2, 4], [6, 12], [3, 2], [4, 0], [3, 2], [5, -2]]
    n = a.size
    h = {}
    line_of = lambda do |p, q|
      y = (p[1] - q[1])
      x = (p[0] - q[0])
      case
      when x == 0 then [p[0], Float::MAX]
      when y == 0 then [Float::MAX, p[1]]
      else
        s = (p[1] - q[1]) * 1.0 / (p[0] - q[0])
        [p[0] - p[1] * 1/s, p[1] - p[0] * s]
      end
    end

    assert_equal [1.75, -7.0], line_of.call([3, 5], [2, 1])
    (0...n).each do |i|
      (i+1...n).each do |j|
        line = line_of.call(a[i], a[j])
        (h[line] ||= {})[a[i]] = 1;
        (h[line] ||= {})[a[j]] = 1;
      end
    end
  end
end
