open! Util
open! Std
open! Schemes
open! Struct
open! Metrics
;;


let shell ~len f x = 
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

24
|> shell ~len:true (Proper.get_all ~max_target_sum:24 ~include_smaller:false)