print_endline "" ;;

let poolB : Nationals.pool = Five ["UVA"; "HARV"; "TXQ"; "UCLA"; "CCC"];;

let poolD : Nationals.pool = Five  ["MICH"; "MIZZ"; "DEIS"; "FSU"; "PSU"];;

let poolFour : Nationals.pool = Four ("D1", "D2b", "D2a");;

let poolW : Nationals.pool = Five  ["WAR"; "LOST"; "TERM"; "SEA"; "PFQC"];;
let poolY : Nationals.pool = Five  ["CAV"; "BPAN"; "SLICE"; "VIP"; "CHIC"];;

let poolZ : Nationals.pool = Five ["BOOM"; "DCQC"; "BAY"; "OSK"; "SWA"];;

let poolX : Nationals.pool = Five ["THC"; "TCQC"; "BONSY"; "WAS"; "HIGH"];;


let conflicts = Hashtbl.of_seq @@ List.to_seq @@ [
  ("HARV", "BPAN"), 1;
  ("TXQ", "CAV"), 1; 
  ("TXQ", "WAR"), 1;
  ("UCLA", "LOST"), 1;
  ("DEIS", "BPAN"), 1; 
  ("TERM", "MIZZ"), 1;
  ("D1", "WAR"), 2;
  ("D1", "SLICE"), 1;
  ("D1", "TERM"), 1;
  ("D1", "OSK"), 1;
  ("D2a", "BPAN"), 0;
  ("D2b", "BPAN"), 0;
 ("CCC", "BOOM"), 1;
 ("D2b", "DCQC"), 0;
 ("D2b", "BONSY"), 0;
];;


let res = Nationals.get_all_full_schedules conflicts 2 [poolFour; poolB; poolY; poolW;         poolD;    poolZ;    poolX; ];;



List.iter (fun x -> print_endline @@ (Util.Lists.to_string ~new_line:true Nationals.schedule_to_string x); print_endline "") (
  List.map fst res
);;

print_int (List.length @@ res)
(*open Util;;
open Engine;;
open Struct;;

Rand.set_seed () ;;
print_endline "" ;;

module Data = Data.M (Scheme);;
module Simulate = Simulate.M (Scheme);;

print_endline @@ Lists.to_string string_of_int (
  Tree.color (Bracket.to_tree [64;0;0;0;0]) 8
);;
*)




(*
let number_of_teams = 12;;

let specs number_advance = {(Specs.default number_of_teams) with
  number_advance;
  max_games = 8;
};;


List.iter (fun k -> print_int k; print_string " -> "; print_int (List.length (Scheme.get_all (specs k))); print_endline "")
(List.init number_of_teams ((+) 1))
*)
(*List.iter (fun s -> print_endline @@ Scheme.to_string s) (Scheme.get_all specs);;*)