#! /usr/bin/env julia
# Parsing
str = read("./3.in", String)
input = split(str, "\n")[1:end-1]
squares = [
    begin
        # Thanks to u/mserrano!
        id, x, y, w, h = Tuple(map(x -> parse(Int64, x.match), eachmatch(r"\d+", i)));
        (
            id = id,
            # Plus one 'cuz of Julia's indexing starting at one not zero
            idx = (x + 1 : x + w, y + 1 : y + h)
        )
    end
    for i in input
]

# Calculating the fabric with claims per square inch
fabric = zeros(Int64, 1000, 1000)
for s in squares
    v = view(fabric, s.idx...)
    v.+= 1
end

println("Solution for part 1:")
println(length(fabric[fabric .> 1]))

println("Solution for part 2:")
for s in squares
    if sum(fabric[s.idx[1], s.idx[2]] .- 1) == 0
        println(s.id)
    end
end

# Solution 1: 107663
# Solution 2: 1166
