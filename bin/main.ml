open Util;;

Rand.set_seed ();;


Analysis.analyze_scheme ~iters:1000 ~luck:1. (Bracket.make (List.init 68 (
  function | 0 -> 0 | 67 -> 2 | _ -> 1
)));;

(*
let f x = print_endline @@ Bracket.to_string @@ Bracket.make @@ x;;


f [1];;
f [0; 2];;
f  [0; 1; 1; 1; 1; 2];;
f  [0; 0; 0; 7; 2];;
f [0; 0; 1; 3; 3; 6];;
*)