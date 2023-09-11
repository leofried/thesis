open! Util
open! Std
open! Schemes
open! Metrics
;;


Prop_ord.f2
  ~bracket:[16;0;0;0]
  (* ~tiers:[[1];[2];[3];[4];[5];[6]]
  ~scores: (Prop_ord.T.C [
  [0.5; 0.5; 0.7; 0.7; 0.9; 1.0;];
  [0.5; 0.5; 0.7; 0.7; 0.9; 1.0;];
  [0.3; 0.3; 0.5; 0.5; 0.7; 0.8;];
  [0.3; 0.3; 0.5; 0.5; 0.7; 0.8;];
  [0.1; 0.1; 0.3; 0.3; 0.5; 0.6;];
  [0.0; 0.0; 0.2; 0.2; 0.4; 0.5;];
])
|> string_of_bool
|> print_endline *)
  

(* Prop_ord.run [2;1;1;0];;
Prop_ord.run [4;0;0];; *)

(* Prog.run () *)


(* 

let bracket = [6; 1; 0; 0]
let tiers = Tiers.T.make [1; 2; 4]
let games_played = [
  (0, 0), (1, 0);
  (0, 0), (1, 1);
  (1, 0), (2, 0);
  (1, 0), (2, 1);
  (1, 1), (2, 2);
  (1, 1), (2, 3);
];;

Tiers.f tiers bracket 0.75 games_played
|> List.sexp_of_t sexp_of_float
|> Sexp.to_string
|> print_endline
;;
 *)


(* 

type t = {mutable value : t option} [@@deriving sexp];;


let b = {value = None};;
let a = {value = None};;
b.value <- Some a;


b
|> sexp_of_t
|> Sexp.to_string
|> print_endline;;

a.value <- Some b;

print_endline (if (Some a = Some b) then  "yes" else "no") *)


(* let shell ~len f x = 
  if len then
    x
    |> Debug.time f 
    |> List.length
    |> Debug.print string_of_int
  else
    x
    |> Debug.time f 
    |> List.sexp_of_t (List.sexp_of_t sexp_of_int)
    |> Sexp.to_string
    |> print_endline
    |> Fun.const 0
;;

(* 5
|> List.create
|> List.map (Fun.const 1)
|> shell Tiers.get_all *)

14
|> Debug.time (Proper.get_all ~max_target_sum:1 ~include_smaller:false)
|> Debug.time (List.filter Tiers.(check_proper_bracket Strong (T.make [1;1;2;2;4;4])))
|> List.sexp_of_t (List.sexp_of_t sexp_of_int)
|> Sexp.to_string
|> print_endline
 *)
