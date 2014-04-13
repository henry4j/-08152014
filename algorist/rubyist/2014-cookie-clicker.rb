#!/usr/bin/env /usr/local/bin/jruby

# %w{test/unit open-uri}.each { |e| require e }
%w{open-uri}.each { |e| require e }

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      c, f, x = io.readline.chomp.split.map { |e| e.to_f }
      [tc, c, f, x]
    end
    cases.each { |e| puts solve(*e) }
  end

  def self.solve(tc, c, f, x)
    max = x/2
    map = lambda do |e, g|
      a = x/g
      if e + a > max
        max
      elsif a < c/g
        e + a
      else
        b = map.call(e + c/g, g+f)
        [e + a, b].min
      end
    end
    s = map.call(0, 2.0)
    "Case ##{tc}: #{s}"
  end
end

CodeJam.main(STDIN)