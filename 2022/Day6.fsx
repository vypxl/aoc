#! /usr/bin/env -S dotnet fsi
#load "util.fsx"

let parse = Seq.toList

let solve n = ((+) n) << List.findIndex (((=) n) << List.length << List.distinct) << List.windowed n
let part1 = solve 4
let part2 = solve 14

let input = parse <| System.IO.File.ReadAllText "6.in"

printfn "Solution for part 1: %A" (part1 input)
printfn "Solution for part 2: %A" (part2 input)

// Solution part 1: 1598
// Solution part 2: 2414
