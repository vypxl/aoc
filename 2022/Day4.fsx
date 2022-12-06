#! /usr/bin/env -S dotnet fsi
#load "util.fsx"
open Util
open System.Linq

let parse = Seq.map ((fun [a;b;c;d] -> ((a, b), (c, d))) << pnums) << lines

let contains (a, b) (c, d) = a <= c && b >= d
let intersects (a, b) (c, d) = (not << Seq.isEmpty) <| [a..b].Intersect([c..d])

let part1 = count (fun (a, b) -> contains a b || contains b a)
let part2 = count (fun (a, b) -> intersects a b)

let input = parse <| System.IO.File.ReadAllText "4.in"

printfn "Solution for part 1: %A" (part1 input)
printfn "Solution for part 2: %A" (part2 input)

// Solution part 1: 450
// Solution part 2: 837
