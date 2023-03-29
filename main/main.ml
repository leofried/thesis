open Util;;
open Engine;;
open Struct;;

Rand.set_seed () ;;
print_endline "" ;;

module Data = Data.M (Scheme);;
module Simulate = Simulate.M (Scheme);;


let number_of_teams = 12;;

let specs number_advance = {(Specs.default number_of_teams) with
  number_advance;
  max_games = 8;
};;


List.iter (fun k -> print_int k; print_string " -> "; print_int (List.length (Scheme.get_all (specs k))); print_endline "")
(List.init number_of_teams ((+) 1))

(*List.iter (fun s -> print_endline @@ Scheme.to_string s) (Scheme.get_all specs);;*)