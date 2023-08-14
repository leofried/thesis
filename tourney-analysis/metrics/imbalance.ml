open! Util
open! Std
open! Struct
open! Schemes
(*
type t = int list [@@deriving sexp];;

let kind = "imbalance";;

let empty (specs : Specs.t) : t = 
  if specs.fidel <> 0. then invalid_arg "Cannot create instance of imbalance.ml with nonzero fidelity" else
  specs.number_of_teams 
  |> List.create 
  |> List.map (Fun.const 0)
;;

let fold (t : t) (specs : Specs.t) (scheme : Scheme.t) : t =
  let teams = Team.create_n specs (Scheme.number_of_teams scheme) in
  let results = Scheme.run scheme specs teams in

  let w = List.index (List.hd results) teams in
  List.mapi (fun i n -> if i = w then n + 1 else n) t
;;

let combine : t -> t -> t = List.combine_map (+);;

let score (t : t) = 
  let count = List.fold_left (+) 0 t in
  Math.divide_float_int (Stats.stdev (Stats.of_list (List.map float_of_int t))) count,
  0.,
  count
;;*)