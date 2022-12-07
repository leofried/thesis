open Util;;
Rand.set_seed () ;;



let f ~n ~iters_pow ~pool_counts ~max_games =
  let all_brackets = List.map Bracket.make (List.flatten (Bracket.get_all_brackets n)) in
  print_endline @@ "Brackets Enumerated: " ^ Int.to_string (List.length all_brackets);
  let schemes =
    pool_counts
    |> List.map (fun pool_count -> List.map (Pool_play.make n ~pool_count) all_brackets)
    |> List.flatten
    |> List.filter (fun s -> Scheme.max_games s <= max_games)
  in
  print_endline @@ "Formats Enumerated: " ^ Int.to_string (List.length schemes);
  Analysis.analyze_schemes ~iters:(Math.pow 10 iters_pow) schemes
;;


f ~n:8 ~iters_pow:6 ~pool_counts:[2;3;] ~max_games:4;;
(*f ~n:12 ~iters_pow:5 ~pool_counts:[1;2;3;4;5;6;12] ~max_games:8;;*)
(*f ~n:24 ~iters_pow:2 ~pool_counts:[3;4;5;6] ~max_games:9;;*)