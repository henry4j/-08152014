#!/usr/bin/env /usr/local/bin/ruby

%w{test/unit open-uri}.each { |e| require e }

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      board = 4.times.map { io.readline.chomp }.join
      io.readline unless io.eof?
      [tc, board]
    end
    cases.each { |e| puts status(*e) }
  end

  def self.status(tc, board)
    t0 = 'T'[0]
    x0 = 'X'[0]
    q = nil
    w = @@lines.any? do |l|
      q = nil
      l.all? do |e|
        if board[e] != '.'[0]
          q = board[e] if q.nil? && board[e] != t0
          q && board[e] == q || board[e] == t0
        end
      end
    end
    if w then "Case ##{tc}: #{q == x0 ? 'X' : 'O' } won"
    elsif board.index('.') then "Case ##{tc}: Game has not completed"
    else "Case ##{tc}: Draw"
    end
  end

  @@lines = [
    [ 0,  1,  2,  3], 
    [ 4,  5,  6,  7],
    [ 8,  9, 10, 11],
    [12, 13, 14, 15],
    [ 0,  4,  8, 12],
    [ 1,  5,  9, 13],
    [ 2,  6, 10, 14],
    [ 3,  7, 11, 15],
    [ 0,  5, 10, 15],
    [ 3,  6,  9, 12]
  ]
end

class TestCases < Test::Unit::TestCase
  def test_main
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/rubyist/tic-tac-toe-tomek-testcases/unit-test.in'
    open(test_case_uri) { |io| CodeJam.main(io) }
  end
end

# CodeJam.main(STDIN)
