#!/usr/bin/env ruby
# coding: utf-8

require 'logger'
require 'open-uri'
require 'sqlite3'
require 'date'
require 'optparse'
require 'fileutils'
require 'stringio'

DEFAULT_RANGE = 1391..2050
DEFAULT_DATABASE = "ko-lunar.db"

#
# http://astro.kasi.re.kr/Life/ConvertMonthlyForm.aspx?MenuID=111
#
$log = Logger.new(STDERR)
$log.level = Logger::ERROR

def usage(program_name)
  print <<EOF
Dump the Korean lunar table into sqlite3 database
Usage: #{program_name} [options]

  -o, --output=DATABASE    Set the database filename to DATABASE.
                           Default: #{DEFAULT_DATABASE}

  -r, --range=RANGE        Set the year range of the dump
                           Default: #{DEFAULT_RANGE}

  -v, --verbose            Show the SQL before execution

      --help               Show help message and exit
      --log-level=LEVEL    Set the log level (for maintenance)

EOF
end

def parse_range(range_spec, min, max)
  toks = range_spec.split("-", 2)

  if toks.length == 1
    toks = [ toks[0], toks[0] ]
  end

  toks[0] = min.to_s if toks[0] == ""
  toks[1] = max.to_s if toks[1] == ""

  toks = toks.map { |s| s.to_i }

  if toks[0] >= min and toks[0] <= toks[1] and toks[0] <= max and
      toks[1] >= min and toks[0] <= toks[1] and toks[1] <= max
    return Range.new(*toks)
  else
      raise ArgumentError, "valud must be between #{min}..#{max}"
  end
end

$options = {}
$options[:range] = DEFAULT_RANGE
$options[:database] = DEFAULT_DATABASE
$options[:loglevel] = Logger::ERROR

op = OptionParser.new do |opts|
  opts.banner = "Usage: #{0} [options]"

  opts.on("-o", "--output=[DATABASE]") do |arg|
    $options[:database] = arg
  end
  opts.on("-r", "--range=[RANGE]") do |arg|
    $options[:range] = parse_range(arg, 1391, 2050)
  end

  opts.on("--help") do
    usage(opts.program_name)
    exit 0
  end

  opts.on("-v", "--verbose") do
    $options[:verbose] = true
  end

  opts.on("--log-level=[LEVEL]") do |arg|
    { "DEBUG" => Logger::DEBUG,
      "INFO" => Logger::INFO,
      "WARN" => Logger::WARN,
      "ERROR" => Logger::Error,
      "FATAL" => Logger::FATAL }.each do |k, v|
      $options[:loglevel] = v if k.start_with?(arg.upcase)
    end
  end
end

begin
  op.parse!
rescue OptionParser::InvalidOption => e
  STDERR.write("error: #{e}\n")
  STDERR.write("Try `#{op.program_name} --help' for more information.\n")
  exit 1
end

$log.level = $options[:loglevel]

if not File.readable?($options[:database])
  STDERR.write("error: cannot open the database #{$options[:database]}\n")
  exit 1
end

$database = SQLite3::Database.new($options[:database])

%w(TERM INT QUIT).each { |signame| trap(signame) {
    STDERR.write("Got #{signame} signal, quit\n")
    $database.close if $database
    exit 1
  }
}

$log.debug { "YEAR RANGE: #{$options[:range]}" }
$log.debug { "PRETEND: #{if $options[:pretend] then true else nil end}" }
$log.debug { "DATABASE: #{$options[:database]}" }


print <<EOF
(defconst korean-lunar-cache
  #s(hash-table size #{(($options[:range].last - $options[:range].first).abs + 1) * 13} test eql data (
EOF

sql = <<SQL
SELECT * FROM korean_lunar WHERE lunar_alt LIKE '%01' AND \
lunar_alt >= '#{$options[:range].first}00000' AND \
lunar_alt <= '#{$options[:range].last}13000';
SQL

#print sql
$database.execute(sql) do | id, sol, sol_alt, lun, lun_alt, leap, text |
  print "#{' ' * 4}#{Date.parse(sol).ajd.to_f} \"#{lun_alt}\"  ; #{sol} #{text}\n"
end

print <<EOF
))
  "Solar to Korean lunar cache")

(provide 'lunar-ko-cache)
EOF
