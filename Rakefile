require 'rake/clean'


task :default => [:generate]

SRC = FileList['script/lunar-ko-*.rb']

SRC.each do |script|
  target = File.basename(script.sub(/\.rb$/, ".el"))
  CLOBBER.include(target)
  file target do 
    puts "Generating #{target} from #{script}..."
    sh "#{script} --database=script/ko-lunar.db > #{target}"
  end

  task :generate => target
end

