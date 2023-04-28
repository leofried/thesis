open Util;;
open Engine;;
open Struct;;
open Infix;;

Rand.set_seed () ;;
print_endline "" ;;

module Data = Data.M (Scheme);;
module Simulate = Simulate.M (Scheme);;

(*
let teams = 
  List.map (fun x -> List.map (fun k -> k, x) ["A"; "B"; "C"; "D"]) [1;2;3];;
let f (x, _) (y, _) = if x = y then Dyadic.one else Dyadic.zero;; 
let bracket = List.hd @@ Bracket.to_tree [8;4;0;0;0];;*)

let teams = [
  [5, 1];
  [
    4, 1;
    4, 2;
  ];
  [
    3, 1;
    3, 2;
    3, 3;
  ];
  [
    2, 1;
    2, 3;
    2, 5;
    2, 6
  ];
  [
    1, 1;
    1, 5;
    1, 9;
    1, 11;
  ]
];;

let rec f (r1, i1) (r2, i2) =
  if r2 < r1 then f (r2, i2) (r1, i1) else
    let width = Math.pow 2 (r2 - r1) in
    if (i2 - 1) * width + 1 <= i1 && i1 <= i2 * width then
      (*print_int r1; print_int i1; print_int r2; print_int i2; print_endline " ";*)
      (1, (r2 - r1))
    else
      Dyadic.zero
    ;; 

let bracket = List.hd @@ Bracket.to_tree [4;4;5;0;0;1;0];;


print_endline @@ Lists.to_string ~new_line:true 
  (Tuple.to_string (Lists.to_string (fun (a, b) -> Debug.alpha a ^ string_of_int b)) (string_of_float >>@ Dyadic.to_float))
  (Treecolor2.get_best_bracket bracket f teams)



(*
let teams = ["E1", "E"; "E2", "E"] :: List.map (fun i -> List.map (fun g -> g ^ i, g) ["A"; "B";"C";"D"]) ["1"; "2"; "3";"4"];; 
print_int @@ List.length @@ Treecolor2.get_all teams;;





let tree = List.hd (Bracket.to_tree [4;14;0;0;0;0])
let teams = [["B1", "B"; "A1", "A"]; ["A2", "A"]];;

print_endline @@ Lists.to_string Fun.id (Treecolor.color tree teams)


print_endline @@ Lists.to_string string_of_int (
  (*List.filter (fun x -> x =1  || x =2) @@ *)
   Tree.color (Bracket.to_tree [16; 0; 0; 0; 0]) 4
);;


let number_of_teams = 24;;

let specs = {(Specs.default number_of_teams) with
  number_advance = 1;
  max_games = 8;
};;

Data.print specs;;

Debug.loop (fun () -> print_endline "hi"; Simulate.simulate_schemes specs 1000000 (Scheme.get_all specs));;*)