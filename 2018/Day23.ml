#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries, pcre"]
--
open Batteries
open Printf


let distnn [a; b; c; _] [d; e; f; _] = (abs (a - d)) + (abs (b - e)) + (abs (c - f));;
let distpn [a; b; c] [d; e; f; _] = (abs (a - d)) + (abs (b - e)) + (abs (c - f));;
let distpp [a; b; c] [d; e; f] = (abs (a - d)) + (abs (b - e)) + (abs (c - f));;
let nanosinrangeof nano = let [x; y; z; r] = nano in List.filter (fun n -> (distnn nano n) <= r)
let nnanosinrangeof nano nanos = List.length (nanosinrangeof nano nanos)

let nnanosinrange point nanos = List.length (List.filter (fun n -> (distpn point n) <= (List.last n)) nanos)

let rec best nanos [x; y; z] = 
    let x1  = nnanosinrange [x + 1; y; z] nanos and
        xm1 = nnanosinrange [x - 1; y; z] nanos and
        y1  = nnanosinrange [x; y + 1; z] nanos and
        ym1 = nnanosinrange [x; y - 1; z] nanos and
        z1  = nnanosinrange [x; y; z + 1] nanos and
        zm1 = nnanosinrange [x; y; z - 1] nanos in
    let lst = [x1; xm1; y1; ym1; z1; zm1] in
    let m = List.max lst in
    if (x1 = m && y1 = m && z1 = m && xm1 = m && ym1 = m && zm1 = m) then
      (m, x, y, z, (distpp [x; y; z] [0; 0; 0]))
    else
      match m with
          x1  -> best nanos [x + 1; y; z] 
        | xm1 -> best nanos [x - 1; y; z] 
        | y1  -> best nanos [x; y + 1; z] 
        | ym1 -> best nanos [x; y - 1; z] 
        | z1  -> best nanos [x; y; z + 1] 
        | zm1 -> best nanos [x; y; z - 1] ;;
    

let main =
  (* Setup *)
  let filelines = File.lines_of "23.in" and
  rex = Pcre.regexp "[^-\\d]+" in
  let nanos = List.map (fun line -> 
  let numss = Pcre.split ~rex line in
  List.map (Int.of_string) (List.filter (fun x -> x <> "") numss)
  ) (List.of_enum filelines) in
  
  (* Part 1 *)
  let strongest = List.reduce (fun acc n -> if (List.last acc) < (List.last n) then n else acc) nanos in
  let inrange = nnanosinrangeof strongest nanos in
  printf "Solution for part 1:\n%d\n" inrange;
  
  (* Part 2 *)
    (* My intention was to just try some values out and find
     the local maximum via best and just take the lowest I can find.
     You have to try hard to get the solution but eventually you find it*)
    Random.self_init ();
    let gen _ = (best nanos [((Random.int 199999999) - 100000000); ((Random.int 199999999) - 100000000); ((Random.int 199999999) - 100000000)]) in
    let xs = (Enum.map (gen) (1--10000)) in
    let (mm, _, _, _, dd) = Enum.reduce (fun acc n -> let (m, _, _, _, _) = acc and (nm, _, _, _, _) = n in if m > nm then acc else n) xs in
    printf "%d %d" mm dd;
    Enum.iter (fun p -> let (m, x, y, z, d) = p in if mm = m then printf "%d %d %d %d %d" d m x y z) xs
    
;;

(* Solution part 1: 294 *)
(* Solution part 2: 88894457 *)
