open Util;;
Rand.set_seed () ;;

let pareto = ref false;;
let number_of_teams = ref 0;;
let luck = ref 1.;;
let max_games = ref Int.max_int;;

let spec_list =
  [
    ("-pareto", Arg.Set pareto, "Output a list of pareto optimal formats with the described characteristics.");
    ("-number_of_teams", Arg.Set_int number_of_teams, "The number of teams in the formats (default 0).");
    ("-luck", Arg.Set_float luck, "The luck factor in the formats (default 1.0).");
    ("-max_games", Arg.Set_int max_games, "The maximum number of games in the tournament (default inf).");
  ]
;;

let () = Arg.parse spec_list (fun _ -> ()) "dune exec -- tourney_format [-pareto] [-number_of_teams <n>] [-luck <l>] [-max_games <m>]";;

if !pareto then
  Analysis.pareto_report
    ~luck: !luck
    ~number_of_teams: !number_of_teams
    ~max_games: !max_games
;;



(*

let f ~n ~iters_pow ~pool_counts ~max_games =
  let all_brackets = List.map Bracket.make (List.flatten (Bracket.get_all_brackets n)) in
  print_endline @@ "Brackets Enumerated: " ^ Int.to_string (List.length all_brackets);
  let schemes =
    pool_counts
    |> List.map (fun pool_count -> List.map (Pool_play.make n ~pool_count) all_brackets)
    |> List.flatten
    |> List.filter (fun (s : Scheme.t) -> s.max_games <= max_games)
  in
  print_endline @@ "Formats Enumerated: " ^ Int.to_string (List.length schemes);
  Analysis.analyze_schemes ~iters:(Math.pow 10 iters_pow) schemes
;;

(*f ~n:8 ~iters_pow:4 ~pool_counts:[1;2;3;4;8] ~max_games:4;*)
(*f ~n:12 ~iters_pow:6 ~pool_counts:[1;2;3;4;5;6;12] ~max_games:8;;*)
(*f ~n:24 ~iters_pow:6 ~pool_counts:[3;4;5;6] ~max_games:9;;*)

Analysis.pareto_report ~number_of_teams:12 ~max_games:2 ();;

(*command line args*)  *)