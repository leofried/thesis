open Util;;
open Engine;;
open Struct;;

Rand.set_seed () ;;
print_endline "" ;;

module Data = Data.M (Scheme);;
module Simulate = Simulate.M (Scheme);;

(*

print_endline @@ Lists.to_string string_of_int (
  (*List.filter (fun x -> x =1  || x =2) @@ *)
   Tree.color (Bracket.to_tree [16; 0; 0; 0; 0]) 4
);;
*)

let tree = List.hd (Bracket.to_tree [4;14;0;0;0;0])
let teams = ["E1", "E"; "E2", "E"] :: List.map (fun i -> List.map (fun g -> g ^ i, g) ["A"; "B";"C";"D"]) ["1"; "2"; "3";"4"];; 

print_endline @@ Lists.to_string Fun.id (Treecolor.color tree teams)

(*
let number_of_teams = 24;;

let specs = {(Specs.default number_of_teams) with
  number_advance = 1;
  max_games = 8;
};;

Data.print specs;;

Debug.loop (fun () -> print_endline "hi"; Simulate.simulate_schemes specs 1000000 (Scheme.get_all specs));;*)