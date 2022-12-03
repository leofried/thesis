open Util;;
Rand.set_seed () ;;


let iters = Math.pow 10 5;;

for n = 1 to 8 do
  Analysis.analyze_schemes ~iters (List.map 
    (fun bracket -> Pool_play.make n ~pool_count:2 (Bracket.make bracket))
    (List.flatten (Bracket.get_all_brackets n))
  );
  print_endline "";
done;;