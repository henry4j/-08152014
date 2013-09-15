require 'irb/completion'
ARGV.concat [ '--readline', '--prompt-mode', 'simple' ]

module Readline
  module History
    LOG = "#{ENV['HOME']}/.irb-history"

    def self.write_log(line)
      File.open(LOG, 'ab') { |f| f << "#{line}\n" }
    end

    def self.start_session_log
      write_log("### session begins: #{Time.now}\n")
      at_exit { write_log("### session ends: #{Time.now}\n") }
    end
  end

  alias :old_readline :readline
  def readline(*args)
    line = old_readline(*args)
    begin
      History.write_log(line)
    rescue
    end
    line
  end
end

Readline::History.start_session_log

require 'irb/ext/save-history'
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-save-history"
IRB.conf[:PROMPT_MODE] = :SIMPLE
