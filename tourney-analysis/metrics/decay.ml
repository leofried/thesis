open! Util;;
open! Std;;

type t = int [@@deriving sexp];;
type u = Stats.t [@@deriving sexp];;

let kind = "decay";;

let empty (_ : t) = Stats.empty;;

let fold m u ~teams ~results =
  let top =
    List.top_of_list m
    >> Pair.left
    >> List.map Team.skill
    >> List.fold_left (+.) 0.
  in
  Stats.add_sample u (top (Team.sort teams) -. top results)
;;

let combine (_ : t) u1 u2 = Stats.combine u1 u2;;

let score (_ : t) u = Stats.mean u, Stats.stderr u;;