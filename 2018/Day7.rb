#! /usr/bin/env ruby
f = File.read("7.in").split("\n")

def println(x)
    print x
    puts
end

def get_deps(f)
    deps = {}
    for l in f do
        a, b = /Step ([A-Z]) must be finished before step ([A-Z]) can begin./.match(l).captures
        deps[a] = [] unless deps.has_key? a
        deps[b] = [] unless deps.has_key? b
        deps[b] << a
    end
    deps
end

deps = get_deps f

q = deps.keys.select { |k| deps[k].empty? }
order = []

until q.empty?
    cur = q.sort[0]
    deps.keys.each { |k| deps[k].delete cur }
    order << cur
    q = deps.keys.select { |k| deps[k].empty? }
    q -= order
end

puts "Solution for part 1:"
puts order.join("")

deps = get_deps f
q = deps.keys.select { |k| deps[k].empty? }
done = []
workers = (1..5).map { { :op => " ", :time => 32 } }
time = -1

until (deps.keys - done).empty?
    free = workers.select { |w| w[:time] >= w[:op].ord - 4 }
    free.each { |w| done << w[:op] unless done.include? w[:op] }
    deps.keys.each { |k| deps[k] -= done }

    q = (deps.keys.select { |k| deps[k].empty? } - (done + workers.map { |w| w[:op] })).sort
    free.each do |w|
        unless q.empty?
            w[:op] = q.shift
            w[:time] = 0
        end
    end

    workers.each { |w| w[:time] += 1 }
    time += 1

    # puts "#{time}\t#{workers.map { |w| w[:op] }.join "\t"}\t#{done.join}"
end

puts "Solution for part 2:"
puts time

# Solution part 1: GLMVWXZDKOUCEJRHFAPITSBQNY
# Solution part 2: 1105
