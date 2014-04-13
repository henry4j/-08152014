#!/usr/bin/env /usr/local/bin/ruby

%w{test/unit open-uri}.each { |e| require e }

module Search
  def self.backtrack(candidate, expand_out, reduce_off)
    unless reduce_off.call(candidate)
      (expand_out.call(candidate) || []).each do |e|
        candidate.push e
        backtrack(candidate, expand_out, reduce_off)
        candidate.pop
      end
    end
  end
end

module CodeJam
  def self.main(io)
    cases = 1.upto(io.readline.to_i).map do |tc|
      n_keys, n_chests = io.readline.chomp.split.map { |e| e.to_i }
      keys = io.readline.chomp.split.map { |e| e.to_i }
      chest_defs = n_chests.times.map { io.readline.chomp.split.map { |e| e.to_i } }
      [tc, keys, chest_defs]
    end
    cases.each_slice(4).each do |s| 
      # s.map { |e| sort_chests(*e) }.each { |e| puts e }
      s.map { |e| Thread.new { sort_chests(*e) } }.map { |e| e.value }.each { |e| puts e }
    end
  end

  def self.sort_chests(tc, keys, chest_defs)
    answers = []
    open_chest = lambda do |key_ring, c|
      k = chest_defs[c][0]
      chest_defs[c][2..-1].reduce(key_ring.merge(k => key_ring[k] - 1)) { |h, k| h.merge(k => h[k].to_i + 1) }
    end

    reducible = lambda do |a|
      key_ring, chests = *a.last[1..2]
      until (openables = chests.select { |c| key_ring[chest_defs[c][0]].to_i > 0 }).empty?
        openables.each do |c|
          key_ring = chest_defs[c][2..-1].reduce(key_ring) { |h, k| h.merge(k => h[k].to_i + 1) }
          chests -= [c]
        end
      end
      chests.empty?
    end

    reduce_off = lambda do |a|
      a.last[2].empty? and answers << a[1..-1].map { |e| e[0] + 1 } \
      or !answers.empty? or !reducible.call(a)
    end

    expand_out = lambda do |a|
      key_ring, chests = *a.last[1..2]
      chests.select { |c| key_ring[chest_defs[c][0]].to_i > 0 }.
        reduce([]) { |s, c| s << [c, open_chest.call(key_ring, c), chests - [c]] }
    end

    key_ring = keys.reduce({}) { |h, k| h.merge(k => 1 + h[k].to_i) } # counts by key
    all_keys = chest_defs.reduce(key_ring.dup) { |h, c| c[2..-1].reduce(h) { |h, k| h.merge(k => 1 + h[k].to_i) } }
    has_enough_keys = chest_defs.all? { |c| all_keys[c[0]] -= 1 if all_keys[c[0]].to_i > 0 }
    chests = chest_defs.size.times.to_a
    Search.backtrack([[nil, key_ring, chests]], expand_out, reduce_off) if has_enough_keys

    "Case ##{tc}: #{ answers.empty?? 'IMPOSSIBLE' : answers[0].join(' ') }"
  end
end

class TestCases < Test::Unit::TestCase
  def test_main
    test_case_uri = 'https://raw.github.com/henry4j/-/master/algorist/rubyist/treasure-testcases/D-small-practice.in'
    open(test_case_uri) { |io| CodeJam.main(io) }
  end
end

# CodeJam.main(STDIN)
