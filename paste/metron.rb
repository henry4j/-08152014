#!/apollo/bin/env ruby
require 'amazon/bsf'
require 'fileutils'
require 'optparse'

options = {}
OptionParser.new do |opts|
  opts.on("-h", "--hostname HOSTNAME", "hostname") do |v|
    options[:hostname] = v
  end
  opts.on("-t", "--threads NUMBER", Integer, "number of threads") do |v|
    options[:threads] = v
  end
end.parse!

sources = ARGV[0] || '/home/metron/merchant_data_*.txt'
LOG_OUTPUT_INTERVALS_IN_UPDATES = 100
CHECKPOINT_INTERVALS_IN_UPDATES = 12
MAXIMUM_HISTORICAL_UPDATE_ATTEMPTS = 5
INITIAL_RETRY_BACKOFF_MILLISECONDS = 1000
MAXIMUM_HISTORICAL_UPDATES_PER_SEC = 100.0
ACME_SINK_SERVICE_CLIENT = Amazon::BSF::HTTP::Client.new(
  'AcmeSnapshotService', options[:hostname] || 'acme-snapshot-na.amazon.com', 8000, '/'
)
ACME_SINK_SERVICE_CLIENT.default_args = {
  '*CodigoProtocol*' => '1.5',
  '*InterfaceVersion*' => '1.0'
}

def fetch_checkpoint_record(file)
  chk_file = File.join(File.dirname(file.path), 'writable/metron.chk')
  return 2 unless File.exist? chk_file
  chk = open(chk_file) { |f| Marshal.load f }
  return nil if file.mtime < chk[0]
  return nil if File.basename(file.path) < chk[1]
  return 2 if File.basename(file.path) > chk[1]
  return chk[2].to_i
end

def store_checkpoint_record(file)
  tmp_file = '/tmp/metron.tmp'
  chk_file = File.join(File.dirname(file.path), 'writable/metron.chk')
  open(tmp_file, 'w') do |chk|
    Marshal.dump [file.mtime, File.basename(file.path), file.lineno.to_s], chk
  end
  FileUtils.mv tmp_file, chk_file, :force => true
end

def try_update_historical(line)
  backoff_millis = INITIAL_RETRY_BACKOFF_MILLISECONDS
  MAXIMUM_HISTORICAL_UPDATE_ATTEMPTS.times do |i|
    begin
      ACME_SINK_SERVICE_CLIENT.updateMerchantHistorical('historicalDataRawString' => line)
      return true
    rescue
      puts $!.inspect, $@
      sleep(backoff_millis / 1000.0)
      backoff_millis *= 2
      next
     end
  end
  false
end

def try_update_historical_in_parallel(lines)
  tries = lines.map { |l| Thread.new { try_update_historical(l) } }.map { |l| l.value }
  tries.reduce(true) { |lhs, rhs| lhs && rhs }
end

sources = Dir.glob(sources).sort { |a,b|
  comp = File.mtime(a) <=> File.mtime(b) 
  comp.zero? ? File.basename(a) <=> File.basename(b) : comp
}

sources.each do |source|
  updates = 0
  begin_time = Time.now
  open(source) do |f|
    lineno = fetch_checkpoint_record(f)
    puts "begins processing updates in #{source} at #{begin_time} with lineno: #{lineno}."
    next if lineno.nil? || lineno < 0 # next source if we are done.
    until f.eof || f.lineno == lineno - 1 do f.gets end
    until f.eof do
      lines = []
      (options[:threads] || 12).times do 
        lines << f.gets unless f.eof
      end
      exit -1 unless try_update_historical_in_parallel(lines)
      if updates % CHECKPOINT_INTERVALS_IN_UPDATES + lines.length >= CHECKPOINT_INTERVALS_IN_UPDATES
        store_checkpoint_record f
      end
      elapsed_time = Time.now - begin_time
      if updates % LOG_OUTPUT_INTERVALS_IN_UPDATES + lines.length >= LOG_OUTPUT_INTERVALS_IN_UPDATES
        puts "@#{f.lineno}/#{'%.2f' % (updates/elapsed_time)} tps: #{lines[-1]}"
      end
      updates += lines.length
      sleep_seconds = (updates / MAXIMUM_HISTORICAL_UPDATES_PER_SEC) - elapsed_time
      sleep sleep_seconds if sleep_seconds > 0
    end
    f.lineno = -f.lineno # negative lineno indicates that we are done.
    store_checkpoint_record f
  end
  puts "ends processing #{updates} updates in #{source} at #{Time.now}."
end
