#! /usr/bin/env -S dotnet fsi
#load "util.fsx"
open Util
open System

let parse s =
  let [stacksRaw; movesRaw] = superlines s
  let stacks = List.filter (not << List.isEmpty) << List.map (List.filter (Char.IsLetter)) << List.transpose << List.map (Seq.toList) <| lines stacksRaw
  let moves = List.map (tuple3 << nums) <| lines movesRaw
  ([] :: stacks, moves)

let move rev (stacks : char list list) (amount, src, dst) =
  let toMove = (if rev then List.rev else id) <| List.take amount stacks.[src]
  let stackA = List.skip amount stacks.[src]
  let stackB = List.append toMove stacks.[dst]
  stacks |> List.updateAt src stackA |> List.updateAt dst stackB

let sol isPart1 (stacks : char list list, moves) = join << List.map List.head << List.tail <| List.fold (move isPart1) stacks moves

let part1 = sol true // List.fold (move true) stacks moves
let part2 = sol false // List.fold (move false) stacks moves

let input = parse <| System.IO.File.ReadAllText "5.in"

printfn "Solution for part 1: %A" (part1 input)
printfn "Solution for part 2: %A" (part2 input)

// Solution part 1: SHQWSRBDL
// Solution part 2: CDTQZHBRS
