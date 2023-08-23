open! Util
open! Std
open! Schemes
open! Struct
open! Metrics
;;

Prog.run ()


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
|> print_endline *)

