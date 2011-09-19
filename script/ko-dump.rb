#!/usr/bin/env ruby
# coding: utf-8

require 'logger'
require 'open-uri'
require 'sqlite3'
require 'date'
require 'optparse'
require 'fileutils'
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

def db_prepare(database)
  db = SQLite3::Database.new(database)

  sql_create = <<SQL
CREATE TABLE korean_lunar (
       id INTEGER PRIMARY KEY,
       solar TEXT UNIQUE,
       solar_alt TEXT,
       lunar TEXT,
       lunar_alt TEXT UNIQUE,
       leap INTEGER,
       text);

CREATE UNIQUE INDEX solar_index ON korean_lunar ( solar );
CREATE UNIQUE INDEX lunar_index ON korean_lunar ( lunar_alt );
SQL
  begin
    db.execute_batch(sql_create)
  rescue SQLite3::SQLException => e
    # In case that the table already exists, ignore the exception
  end
  db
end


class KoreanSolar2Lunar
  CONVERTER_URL='http://astro.kasi.re.kr/Life/Knowledge/solar2lunar/convert_monthly.php'

  def initialize(solar_year)
    @year = solar_year
  end

  def parse(&record_handler)
    (1..12).each { |month|
      convert_month(@year, month, record_handler)
    }
  end

  def parse_month(month, &record_handler)
    convert_month(@year, month, record_handler)
  end

  def parse_month_file(fd, block)
    while !fd.eof?
      line = fd.readline
      break if line =~ /<tbody>/
    end

    while !fd.eof?
      while line = fd.readline and ! (line =~ /<a href/)
      end
      #print "line: #{line}\n"
      solar1 = />([^>]*)<\/a>/.match(line)[1].strip

      solar2 = if line =~ /<sup>1<\/sup>/
                   # solar1 is the non-leap date,
                   # and this is the leap date.
                   while line = fd.readline and ! (line =~ /<a href/)
                   end
                   #print "line: #{line}\n"
                   solar2 = />([^>]*)<\/a>/.match(line)[1].strip
               end

      while line = fd.readline and ! (line =~ /<a href/)
      end
      lunar = />([^>]*)<\/a>/.match(line)[1].strip

      kanji = if solar2
                while line = fd.readline and ! (line =~ /<td style/)
                end
                fd.readline.chomp.strip
              else
                while line = fd.readline and ! (line =~ /<td>/)
                end
                fd.readline.chomp.strip
              end

      leap = nil

      #$alt_fd.write("#{solar1}\t#{solar2}\t#{lunar}\t#{kanji}\n")
      solar1 = /[0-9-]*/.match(solar1)[0] if solar1
      solar2 = /[0-9-]*/.match(solar2)[0] if solar2

      leap = true if /\(윤\)/.match(lunar)
      lunar = /[0-9-]*/.match(lunar)[0]
                 
      solar1, solar2 = solar2, solar1 if solar2
      block.call(solar1, solar2, lunar, leap, kanji) if block
    end
    
  rescue EOFError => e
    
  end


  def convert_month(year, month, block)
    url="#{CONVERTER_URL}?sol_year=#{year}&sol_month=#{month}"
    $log.debug { "Opening URL: '#{url}'" }
    fd = open(url)
    #print "FD: #{fd}\n"
    #fd.set_encoding("cp949", __ENCODING__)
    fd.set_encoding("cp949", "utf-8")
    parse_month_file(fd, block)
  rescue ArgumentError => e
    # encoding error
    $log.warn { "URL failed on #{year}-#{month}" }
  end
end


#ks = KoreanSolar2Lunar.new(2050)
#ks.parse_month(5) do |s1, s2, l, leap, t|
#  print "l: #{l} #{if leap then "t" else "nil" end} \n"
#end
#exit 0

def usage(program_name)
  print <<EOF
Dump the Korean lunar table into sqlite3 database
Usage: #{program_name} [options]

  -o, --output=DATABASE    Set the database filename to DATABASE.
                           Default: #{DEFAULT_DATABASE}

  -r, --range=RANGE        Set the year range of the dump
                           Default: #{DEFAULT_RANGE}

  -i, --init               Delete the database before start
  -p, --pretend            No actual SQL execution
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
$options[:init] = nil
$options[:loglevel] = Logger::ERROR

op = OptionParser.new do |opts|
  opts.banner = "Usage: #{0} [options]"

  opts.on("-o", "--output=[DATABASE]") do |arg|
    $options[:database] = arg
  end
  opts.on("-r", "--range=[RANGE]") do |arg|
    print "ARG: #{arg}\n"
    $options[:range] = parse_range(arg, 1391, 2050)
  end

  opts.on("-p", "--pretend") do
    $options[:pretend] = true
  end

  opts.on("--help") do
    usage(opts.program_name)
    exit 0
  end

  opts.on("-v", "--verbose") do
    $options[:verbose] = true
  end

  opts.on("-i", "--init") do
    $options[:init] = true
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

FileUtils.rm([ $options[:database] ], :force => true) if $options[:init]

$database = db_prepare($options[:database])

%w(TERM INT QUIT).each { |signame| trap(signame) {
    STDERR.write("Got #{signame} signal, quit\n")
    $database.close if $database
    exit 1
  }
}

$log.debug { "YEAR RANGE: #{$options[:range]}" }
$log.debug { "PRETEND: #{if $options[:pretend] then true else nil end}" }
$log.debug { "DATABASE: #{$options[:database]}" }

$options[:range].each { |year|
#(1391..1391).each { |year|
  STDERR.write("Processing #{year}...")
  s2l = KoreanSolar2Lunar.new(year)
  s2l.parse { |s1, s2, l, leap, t|
    $log.debug { "RECORD: ||#{s1}||#{s2}||#{l}||#{leap}||#{t}||" }

    lunar_alt = l.gsub(/-/, '')

    lunar_alt = lunar_alt[0..5] + if leap then "1" else "0" end + 
                lunar_alt[6..7]

    sql = "INSERT INTO korean_lunar ( solar, solar_alt, \
lunar, lunar_alt, leap, text ) \
VALUES ( '#{s1}', '#{s2}', '#{l}', '#{lunar_alt}', \
#{if leap then 1 else 0 end}, '#{t}' );";

    #print "sql: #{sql}\n"
    begin
      print "#{sql}\n" if $options[:verbose]

      if not $options[:pretend]
        $database.execute(sql);
      end
    rescue SQLite3::ConstraintException => e
      if $options[:init]
        STDERR.write("error: #{e}\n")
        STDERR.write("You need to remove the existing database with `--init'.\n")
        exit 1
      end
    rescue SQLite3::Exception => e
      STDERR.write("error: #{e}\n")
      exit 1
    end
  }
  STDERR.write(" done.\n")
}

