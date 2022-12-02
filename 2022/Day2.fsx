#! /usr/bin/env -S dotnet fsi
#load "util.fsx"
open Util

let parse_line ([a; _; b] : char list) = (int a - int 'A', int b - int 'X')
let parse = Seq.map (parse_line << Seq.toList) << lines

let part1 = Seq.sum << Seq.map (fun (a, b) -> 1 + b + ((b - a + 4) % 3) * 3)
let part2 = part1 << Seq.map (fun (a, b) -> (a, (a + b + 2) % 3))

let input = parse <| System.IO.File.ReadAllText "2.in"

printfn "Solution for part 1: %A" (part1 input)
printfn "Solution for part 2: %A" (part2 input)

// Solution part 1: 11449
// Solution part 2: 13187
