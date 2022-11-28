open Util;;

Rand.set_seed ();;

let iters = 1000000;;
let luck = 1.;;

List.iter
  (fun bracket ->
    Analysis.analyze_scheme ~iters ~luck (Bracket.make bracket)
  )
  (Bracket.get_all_brackets 8)
;;


(*
let k : string = Int_list.to_string (Int_list.to_string Int.to_string) (Bracket.make_all 12);;
print_endline k;;


let f x = print_endline @@ Bracket.to_string @@ Bracket.make @@ x;;


f [1];;
f [0; 2];;
f  [0; 1; 1; 1; 1; 2];;
f  [0; 0; 0; 7; 2];;
f [0; 0; 1; 3; 3; 6];;
*)