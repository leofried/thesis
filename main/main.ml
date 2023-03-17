open Util;;


Rand.set_seed () ;;
print_endline "" ;;

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
flush channel;