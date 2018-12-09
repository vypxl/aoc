#! /usr/bin/env -S bash -c "nimcr \$0 && rm .Day8"
from strutils import split, splitLines, parseInt, intToStr
from sequtils import map, foldl

var
    f = splitLines(readFile("8.in"))[0]
    data = map(split(f, " "), parseInt)

type
    Node = ref object of RootObj
        nchildren*: int
        nmeta*:     int
        children*:  seq[Node]
        meta*:      seq[int]

proc parse(d: seq[int], nc: int, nm: int): (Node, int) =
    var 
        n = Node(nchildren: nc, nmeta: nm, children: @[], meta: @[])
        i = 0
        x = 0
    while x < nc:
        var (child, length) = parse(d[i+2..len(d)-1], d[i], d[i+1])
        n.children.add(child)
        i += 2 + length
        x += 1
    x = 0
    while x < nm:
        n.meta.add(d[i])
        i += 1
        x += 1
    return (n, i)
    
proc sumMeta(n: Node): int =
    var sum = foldl(n.meta, a + b)
    for c in n.children:
        sum += sumMeta(c)
    return sum
        
proc check2(n: Node): int = 
    if n.nchildren == 0:
        return foldl(n.meta, a + b)
        
    var sum = 0
    for m in n.meta:
        if m < 1 or m > n.nchildren:
            continue
        sum += check2(n.children[m - 1])
    return sum

var (tree, _) = parse(data[2..len(data)-1], data[0], data[1])

echo "Solution for part 1:"
echo sumMeta(tree)
echo "Solution for part 2:"
echo check2(tree)

# Solution part 1: 45750
# Solution part 2: 23266
