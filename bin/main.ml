open Util;;
Rand.set_seed () ;;



let iters = 400000;;

for n = 8 to 8 do
  List.iter
    (fun bracket ->
      Analysis.analyze_scheme ~iters (Pool_play.make n (Bracket.make bracket))
    )
    (List.flatten (Bracket.get_all_brackets n))
  ; print_endline "";
done;;