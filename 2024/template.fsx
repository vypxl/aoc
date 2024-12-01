#! /usr/bin/env -S dotnet fsi
#load "util.fsx"
open Util

let parse = id

let part1 inp = inp
let part2 _ = 2

let input = parse <| System.IO.File.ReadAllText "_DAY_.in"

printfn "Solution for part 1: %A" (part1 input)
printfn "Solution for part 2: %A" (part2 input)

// Solution part 1:
// Solution part 2:
