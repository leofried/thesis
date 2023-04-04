open Util;;
open Engine;;
open Struct;;

Rand.set_seed () ;;
print_endline "" ;;

module Data = Data.M (Scheme);;
module Simulate = Simulate.M (Scheme);;
(*print_endline @@ Lists.to_string string_of_int (
  Tree.color (Bracket.to_tree [64;0;0;0;0]) 8
);;*)

let number_of_teams = 24;;

let specs = {(Specs.default number_of_teams) with
  number_advance = 1;
  max_games = 8;
};;

Data.print specs;;
(*
Debug.loop (fun () -> print_endline "hi"; Simulate.simulate_schemes specs 1000000 (Scheme.get_all specs));;*)