open System

[<AutoOpen>]
module Util =
  let split (delim : string) (s : string) : string seq = s.Split(delim, StringSplitOptions.RemoveEmptyEntries)
  let lines = split "\n"
