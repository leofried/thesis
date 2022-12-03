open Util;;
Rand.set_seed () ;;



let f ~n ~iters_pow ~pool_counts ~max_games () =
  let all_brackets = List.flatten (Bracket.get_all_brackets n) in
  let schemes =
    pool_counts
    |> List.map
      (fun pool_count -> List.map 
        (fun bracket -> Pool_play.make n ~pool_count (Bracket.make bracket))
        all_brackets
      )
    |> List.flatten
    |> List.filter (fun s -> Scheme.max_games s <= max_games)
  in
  Analysis.analyze_schemes ~iters:(Math.pow 10 iters_pow) schemes
;;

(*print_float @@ Stats.binom_error ~iters:(Math.pow 10 5) ~cats:24 ();;*)

System.time @@ f ~n:12 ~iters_pow:4 ~pool_counts:[2;3;4] ~max_games:8;;
(*f ~n:24 ~iters_pow:5 ~pool_counts:[3;4;5;6] ~max_games:9;;*)
(*f ~n:6 ~iters_pow:5 ~pool_counts:[1;2;3;6] ~max_games:5;;*)