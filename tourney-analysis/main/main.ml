open! Util
open! Std
open! Schemes
open! Struct
open! Metrics
;;

(*
7
|> List.create
|> List.map (Fun.const 1)
*)
[1;2;3]
|> Proper.get_all
|> List.sexp_of_t (List.sexp_of_t sexp_of_int)
|> Sexp.to_string
|> print_endline;;