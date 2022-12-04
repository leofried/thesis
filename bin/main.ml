open Util;;
Rand.set_seed () ;;



let f ~n ~iters_pow ~pool_counts ~max_games =
  let all_brackets = List.map Bracket.make (List.flatten (Bracket.get_all_brackets n)) in
  print_endline "Brackets Enumerated";
  let schemes =
    pool_counts
    |> List.map (fun pool_count -> List.map (Pool_play.make n ~pool_count) all_brackets)
    |> List.flatten
    |> List.filter (fun s -> Scheme.max_games s <= max_games)
  in
  Analysis.analyze_schemes ~luck:3. ~iters:(Math.pow 10 iters_pow) schemes
;;

(*print_float @@ Stats.binom_error ~iters:(Math.pow 10 5) ~cats:24 ();;*)

f ~n:12 ~iters_pow:6 ~pool_counts:[1;2;3;4;5;6;12] ~max_games:4;;
(*f ~n:24 ~iters_pow:3 ~pool_counts:[3;4;5;6] ~max_games:9;;*)
(*f ~n:9 ~iters_pow:6 ~pool_counts:[1;2;3;4;9] ~max_games:3;;*)