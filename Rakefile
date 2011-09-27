require 'rake/clean'


task :default => [:generate]

SRC = FileList['script/lunar-ko-mdays.rb']

SRC.each do |script|
  target = File.basename(script.sub(/\.rb$/, ".el"))
  CLOBBER.include(target)
  file target do 
    puts "Generating #{target} from #{script}..."
    sh "#{script} --database=script/ko-lunar.db > #{target}"
  end

  task :generate => target
end

CLEAN.include("cache.dat")
task :cachedata => ["script/lunar-ko-cache.rb"] do |t|
  sh "./script/lunar-ko-cache.rb -dscript/ko-lunar.db > cache.dat"
end

file "lunar-ko-cache.el" => [:cachedata] do |t|
  sh "emacs -q -L . --batch -l gencache -f lunar-ko-generate-cache cache.dat lunar-ko-cache.el"
end
task :generate => "lunar-ko-cache.el"
