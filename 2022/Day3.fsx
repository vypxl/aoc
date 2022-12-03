#! /usr/bin/env -S dotnet fsi
#load "util.fsx"
open Util
open System

let parse = Seq.map (seq) << lines

let prio (x : char) = if Char.IsLower x then int x - int 'a' + 1 else int x - int 'A' + 27
let sumCommonPriorities = Seq.sum << Seq.map (prio << Seq.head << Seq.reduce intersect)

let part1 = sumCommonPriorities << Seq.map (parts 2)
let part2 = sumCommonPriorities << chunks 3

let input = parse <| System.IO.File.ReadAllText "3.in"

printfn "Solution for part 1: %A" (part1 input)
printfn "Solution for part 2: %A" (part2 input)

// Solution part 1: 8139
// Solution part 2: 2668
