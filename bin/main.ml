open Util;;
Rand.set_seed () ;;



let iters = 1000000;;

for n = 8 to 8 do
  List.iter
    (fun bracket ->
      Analysis.analyze_scheme ~iters (Pool_play.make n ~pool_count:1 (Bracket.make bracket));
      Analysis.analyze_scheme ~iters (Bracket.make bracket)
    )
    (List.hd (Bracket.get_all_brackets n))
  ; print_endline "";
done;;