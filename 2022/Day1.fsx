#! /usr/bin/env -S dotnet fsi
#load "util.fsx"
open Util

let parse = Seq.rev << Seq.sort << Seq.map (Seq.sum << Seq.map int << lines) << split "\n\n"

let part1 = Seq.head
let part2 = Seq.sum << Seq.take 3

let input = parse <| System.IO.File.ReadAllText "1.in"

printfn "Solution for part 1: %A" (part1 input)
printfn "Solution for part 2: %A" (part2 input)

// Solution part 1: 67450
// Solution part 2: 199357
