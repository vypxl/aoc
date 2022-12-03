open System

[<AutoOpen>]
module Util =
  let split (delim : string) (s : string) : string seq = s.Split(delim, StringSplitOptions.RemoveEmptyEntries)
  let lines = split "\n"
  let chunks k = Seq.map Seq.ofArray << Seq.chunkBySize k
  let parts k xs = chunks (Seq.length xs / k) xs
  let intersect xs ys = Set.intersect (Set.ofSeq xs) (Set.ofSeq ys) |> Set.toSeq
