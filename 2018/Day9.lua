#! /usr/bin/env lua
io.input(io.open("9.in"))
nplayers, lastMarble = string.match(io.read("*line"), "(%d+) players; last marble is worth (%d+) points")
cur = { v = 2 }
cur.n, cur.p = cur
players = {}
for i = 1, nplayers do players[i] = 0 end

for m = 1, lastMarble*100 do
    if m % 23 == 0 then
        for _ = 1,7 do cur = cur.p end
        local p = (m-1) % nplayers + 1
        players[p] = players[p] + m + cur.v
        cur.n.p = cur.p
        cur.p.n = cur.n
        cur = cur.n
    else
        cur = cur.n
        local insert = { p = cur, v = m, n = cur.n }
        cur.n.p = insert
        cur.n = insert
        cur = insert
    end
    
    if m - lastMarble == 0 then
        print("Solution for part 1:")
        local maxScore = 0
        for i, v in pairs(players) do
            if v > maxScore then
                maxScore = v
            end
        end
        print(maxScore)
    end
end

maxScore = 0
for i, v in pairs(players) do
    if v > maxScore then
        maxScore = v
    end
end
print "Solution for part 2:"
print(maxScore)

-- Solution part 1: 416424
-- Solution part 2: 3498287922
