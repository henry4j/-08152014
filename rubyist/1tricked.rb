#!/usr/bin/env ruby
# encoding: utf-8
# Ruby Styles https://github.com/bbatsov/ruby-style-guide

# * Arrays.transpose, quickfind_first, find_occurences, min_out_of_cycle, index_out_of_cycle
# * Strings.min_window_span, index_of_by_rabin_karp, and combine_parens
# * SNode.find_cycle; Numbers.divide, sum, minmax, and abs.
# * Miller-Rabin's primality test, 1 = a**(n-1) % n, when a is in (2..n-2).
# * Data Structures: Trie, BinaryHeap, LRUCache, CircularBuffer, and MedianBag
# * Graph: has_cycle? topological_sort that pushes v onto a stack on vertex exit.
# * Binary Tree: path_of_sum, common_ancestors, diameter, successor, and last(k).
# * integer partition & composition, and set partition by restricted growth string.
# * DP.optimal_tour (TSP), optimal_BST, edit distance, partition_bookshelf, subset sum.

%w{test/unit stringio set}.each { |e| require e }

module Math
  def self.negate(a)
    neg = 0
    e = a < 0 ? 1 : -1
    while a != 0
      a += e
      neg += e
    end
    neg
  end

  def self.subtract(a, b)
    a + negate(b)
  end

  def self.abs(a)
    a < 0 ? negate(a) : a
  end

  def self.multiply(a, b)
    if abs(a) < abs(b)
      multiply(b, a)
    else
      v = abs(b).times.reduce(0) { |v, _| v += a }
      b < 0 ? negate(v) : v
    end
  end

  def self.different_signs(a, b)
    a < 0 && b > 0 || a > 0 && b < 0 ? true : false
  end

  def self.divide(a, b)
    if a < 0 && b < 0
      divide(negate(a), negate(b))
    elsif a < 0
      negate(divide(negate(a), b))
    elsif b < 0
      negate(divide(a, negate(b)))
    else
      quotient = 0
      divisor = negate(b)
      until a < b
        a += divisor
        quotient += 1
      end
      quotient
    end
  end

  def self.integer_of_prime_factors(k, factors = [3, 5, 7])
    queues = factors.map { |e| [e] }
    x = nil
    k.times do
      j = queues.each_index.reduce(0) { |j, i| queues[j][0] < queues[i][0] ? j : i }
      x = queues[j].shift
      (j...queues.size).each { |i| queues[i] << x * factors[i] }
    end
    x
  end
end

###########################################################
# Miscellaneous Modules & Classes: SNode, and DNode
###########################################################

module Kernel
  def enum(*symbols)
    symbols.each { |s| const_set(s, s.to_s) }
    const_set(:DEFAULT, symbols.first) unless symbols.nil?
  end
end

###########################################################
# System and Object-Oriented Design
###########################################################

module JukeboxV1
  # a design consists of key components & their responsibilities, interactions, and services.
  # http://alistair.cockburn.us/Coffee+machine+design+problem,+part+2
  # http://codereview.stackexchange.com/questions/10700/music-jukebox-object-oriented-design-review
  # http://www.simventions.com/whitepapers/uml/3000_borcon_uml.html
  class JukeboxEngine # coordinates all activities.
    def play(track) end; def choose_track() end; def show_welcome() end
    attr_reader :current_account, :current_track
    attr_reader :now_playing, :most_played, :recently_played, :recently_added
    attr_reader :account_dao, :album_dao, :playlist_dao
  end

  class CardReader # reads magnetic account id cards.
    def load_account() end
  end

  class Playlist # keeps track of most-played, recently-played, and now-playing lists.
    def initialize(capacity, mode = :LRU_CACHE) end
    def enqueue(track) end # reordering occurs in LRU-CACHE mode.
    def dequeue() end # bubbling down happens in MIN-HEAP mode.
    def move_up(track) end
    def move_down(track) end
    def size() end
  end

  class AudioCDPlayer # controls actual audio CD player.
    def play(track) end
    def pause() end
    def resume() end
    def stop() end
    def set_volume() end
    def get_volume() end
    def mute() end
    def unmute() end
  end

  class Account # keeps track of user account.
    attr_accessor :credit, :most_played, :recent_played
  end

  class Album # location: a certain tray id.
    attr_reader :label, :artists, :release_date, :genre, :subgenre, :tracks, :location
  end

  class Track # location: a certain track id.
    attr_reader :title, :duration, :location
  end

  class AccountDAO # (same as AlbumDAO and PlaylistDAO)
    def save(entity) end # create and update
    def find(id) end
    def find_all(example = nil) end
    def count(example = nil) end
    def exists?(example = nil) end
    def delete(id) end
    def delete_all(example = nil) end # delete
  end
end

module CardGame
  # http://math.hws.edu/javanotes/c5/s4.html
  class Card
    attr_accessor :suit, :value
  end

  class Deck
    def suffle() end
    def cards_left() end
    def deal_card() end # raises illegal state.
  end

  class Hand
    def add_card(card) end
    def remove_card(card) end # raises no such item found.
    def remove_card(position) end # raises index out of bounds.
    def get_card(position) end
    def clear_cards() end
    def sort_by_suit() end
    def sort_by_value() end
    attr_accessor :cards
  end

  class Game
    def games_played() end
    def scores_achieved() end
    def set_up() end
    def tear_down() end
    attr_accessor :deck, :hands
  end
end

module TetrisV1
  class TetrisEngine
    def set_up() end; def tear_down() end; def clear() end
    def move_piece() end; def update_state() end; def remove_lines; end
    attr_reader :current_piece, :pieces_in_queue, :board_array, :game_state
    attr_reader :current_level, :current_score, :games_palyed, :best_scores
  end

  class TetrisBlock < Struct.new(:shape, :color, :orientation, :location); end

  module TetrisShape # http://www.freewebs.com/lesliev/rubyenums.html
    enum :SQUARE, :LINE, :T, :L, :REVERSE_L, :Z, :REVERSE_Z
  end
end

module XMPP
  # http://en.wikipedia.org/wiki/Extensible_Messaging_and_Presence_Protocol
  # http://weblog.raganwald.com/2006/06/my-favourite-interview-question.html
  # http://www.12planet.com/en/software/chat/whatis.html
  # http://tianrunhe.wordpress.com/2012/03/21/design-an-online-chat-server/
  # http://tianrunhe.wordpress.com/tag/object-oriented-design/
  # http://tianrunhe.wordpress.com/2012/03/page/2/
  # http://www.thealgorithmist.com/archive/index.php/t-451.html?s=b732f53954d644b5ebacc77460396c19
  # https://www.google.com/#q=interview+chat+server+design&hl=en&prmd=imvns&ei=lhM3UK33A8SQiQLO-4CAAQ&start=90&sa=N&bav=on.2,or.r_gc.r_pw.r_cp.r_qf.&fp=f0056a833a456726&biw=838&bih=933
end
