#nowarn "25"
open System
open System.Text.RegularExpressions

[<AutoOpen>]
module Util =
  let split (delim : string) (s : string) : string list = Seq.toList <| s.Split(delim, StringSplitOptions.RemoveEmptyEntries)
  let lines = split "\n"
  let superlines = split "\n\n"
  let chunks k = Seq.map Seq.ofArray << Seq.chunkBySize k
  let parts k xs = chunks (Seq.length xs / k) xs
  let intersect xs ys = Set.intersect (Set.ofSeq xs) (Set.ofSeq ys) |> Set.toSeq
  let findAll re str = Seq.map (fun (x : Match) -> x.Captures[0].Value) (Regex(re).Matches(str))
  let nums = Seq.toList << Seq.map int << findAll "-?\d+"
  let pnums = Seq.toList << Seq.map int << findAll "\d+"
  let count f = Seq.length << Seq.filter f
  let tuple2 [a;b] = (a, b)
  let tuple3 [a;b;c] = (a, b, c)
  let join (xs : 'a seq) = String.concat "" <| Seq.map string xs
