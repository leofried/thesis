open Util;;
open Struct;;


Rand.set_seed () ;;
print_endline "" ;;



let grid = [
  [4; 2; 2; 0; 0];
  [3; 2; 1; 0];
  [3; 2; 1; 0];
  [4; 2; 0; 0];
  [4; 2; 0; 0];
];;

Pool_play.(verify @@ transform grid);;



(*

let lame = Scheme.Chain [
  Pools (4, Round_robin);
  Bracket [8; 4; 0; 0; 0]
]

let default = Scheme.Chain [
  Pools (5, Round_robin);
  Bracket [6; 5; 0; 0; 0]
];;

let overcorrection = Scheme.Chain [
  Pools (5, Round_robin);
  Bracket [14; 1; 0; 0; 0]
];;

let crossover = Scheme.Chain [
  Pools (5, Round_robin);
  Offset (1, Bracket [4;0;0]);
  Bracket [4; 8; 3; 0; 0; 0]
];;

let formats = [overcorrection; default];;

Simulate.simulate_schemes (Data.default_specs 21) 000000 formats;;

List.iter (fun (s, st) ->
  print_endline @@ Scheme.name s;
  print_endline @@ Math.to_pct ~digits:2 (Stats.mean st);
  print_endline @@ Math.to_pct ~digits:2 (Stats.stderr st);
) @@ Data.read (Data.default_specs 21);;


*)


(*
let number_of_teams = 21;;
let max_games = 8;;

let all = Equity.make_all ~number_of_teams ~max_games () ;;

let path = "analysis/equity-signatures/" ^ string_of_int number_of_teams ^ "teams" ^ string_of_int max_games ^ "games.txt";;
let channel = Out_channel.open_bin path in
Out_channel.output_string channel "Minimum Team Equity | Equity Signature";
List.iter (fun s ->
  let str = "\n" ^ string_of_float (List.hd (Equity.team_equities s)) ^ "  |  " ^ 
  (Lists.to_string (fun (x, y) -> "(" ^ string_of_int x ^ ", " ^ string_of_float (Adic.to_float y) ^ ")") false s) in
  Out_channel.output_string channel str;
) all;
flush channel;*)