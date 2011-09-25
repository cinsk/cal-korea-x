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
# Make sure that the parsing function can handle following date
#
# 2050-05  esp. solar 2050-05-21
#
$log = Logger.new(STDERR)
$log.level = Logger::ERROR

def usage(program_name)
  print <<EOF
Dump the Korean lunar table into sqlite3 database
Usage: #{program_name} [options]

  -d, --database=DATABASE  Set the database filename to DATABASE.
                           Default: #{DEFAULT_DATABASE}

  -r, --range=RANGE        Set the year range of the dump
                           Default: #{DEFAULT_RANGE}

  -v, --verbose            Show the SQL before execution

      --help               Show help message and exit
      --log-level=LEVEL    Set the log level (for maintenance)

EOF
end

sexagesimal_names = {
  "갑자" => 0,
  "을축" => 1,
  "병인" => 2,
  "정묘" => 3,
  "무진" => 4,
  "기사" => 5,
  "경오" => 6,
  "신미" => 7,
  "임신" => 8,
  "계유" => 9,
  "갑술" => 10,
  "을해" => 11,
  "병자" => 12,
  "정축" => 13,
  "무인" => 14,
  "기묘" => 15,
  "경진" => 16,
  "신사" => 17,
  "임오" => 18,
  "계미" => 19,
  "갑신" => 20,
  "을유" => 21,
  "병술" => 22,
  "정해" => 23,
  "무자" => 24,
  "기축" => 25,
  "경인" => 26,
  "신묘" => 27,
  "임진" => 28,
  "계사" => 29,
  "갑오" => 30,
  "을미" => 31,
  "병신" => 32,
  "정유" => 33,
  "무술" => 34,
  "기해" => 35,
  "경자" => 36,
  "신축" => 37,
  "임인" => 38,
  "계묘" => 39,
  "갑진" => 40,
  "을사" => 41,
  "병오" => 42,
  "정미" => 43,
  "무신" => 44,
  "기유" => 45,
  "경술" => 46,
  "신해" => 47,
  "임자" => 48,
  "계축" => 49,
  "갑인" => 50,
  "을묘" => 51,
  "병진" => 52,
  "정사" => 53,
  "무오" => 54,
  "기미" => 55,
  "경신" => 56,
  "신유" => 57,
  "임술" => 58,
  "계해" => 59 }

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

  opts.on("-d", "--database=[DATABASE]") do |arg|
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
(defconst korean-lunar-months
  #s(hash-table size #{$options[:range].count} test eql data (
EOF

$options[:range].each do |year|
#(1391..1391).each { |year|
  sql = "SELECT * FROM korean_lunar WHERE lunar_alt LIKE '#{year}%' ORDER BY lunar_alt ASC;"

  prev_ym = ""
  mdays = 0
  ydays = 0
  leap_month = nil
  kanji_day = nil

  print "    #{year} "

  strio = StringIO.new

  $database.execute(sql) do | id, sol, sol_alt, lun, lun_alt, leap, text |
    ym = lun_alt[0..6]

    if prev_ym != ym
      #print "#{prev_ym[0..3]}-#{prev_ym[4..5]}-#{prev_ym[6]} #{mdays}\n" if prev_ym != ""
      if not kanji_day
        m = / ([^ ]*)\([^)]*\)일/.match(text)
        kanji_day = sexagesimal_names[m[1]]
        #print "KANJI(#{kanji_day}) "
      end
      strio << " #{mdays}" if prev_ym != ""

      leap_month = ym[4..5].to_i if ym[6] != "0"
      #print "prev_ym[4..5] = #{prev_ym[4..5]}, LEAP: #{leap}\n"

      prev_ym = ym
      mdays = 0
    end

    mdays += 1
    ydays += 1
  end
  #print "#{prev_ym[0..3]}-#{prev_ym[4..5]}-#{prev_ym[6]} #{mdays}\n"
  #print "#{prev_ym[0..3]} #{ydays}\n\n"

  #print " #{mdays} [#{lmon}]\n"
  strio << " #{mdays}"

  printf "(%3s", if leap_month then leap_month else "nil" end
  print " #{kanji_day} . [#{strio.string.strip}])\n"

end

print <<EOF
))
  "days in month distribution table")

(provide 'lunar-ko-mdays)
EOF
