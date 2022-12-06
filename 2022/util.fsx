open System
open System.Text.RegularExpressions

[<AutoOpen>]
module Util =
  let split (delim : string) (s : string) : string seq = s.Split(delim, StringSplitOptions.RemoveEmptyEntries)
  let lines = split "\n"
  let chunks k = Seq.map Seq.ofArray << Seq.chunkBySize k
  let parts k xs = chunks (Seq.length xs / k) xs
  let intersect xs ys = Set.intersect (Set.ofSeq xs) (Set.ofSeq ys) |> Set.toSeq
  let findAll re str = Seq.map (fun (x : Match) -> x.Captures[0].Value) (Regex(re).Matches(str))
  let nums = Seq.toList << Seq.map int << findAll "-?\d+"
  let pnums = Seq.toList << Seq.map int << findAll "\d+"
  let count f = Seq.length << Seq.filter f
