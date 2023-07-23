open! Util;;
open! Std;;

type t = int [@@deriving sexp];;
type u = Stats.t [@@deriving sexp];;

let kind = "decay";;

let empty (_ : t) = Stats.empty;;

let fold t u ~teams ~results =
  let real =
    results
    |> List.map (fun t -> t.Team.skill)
    |> List.top_of_list t
    |> Pair.left
    |> List.fold_left (+.) 0.
  in

  let best =
    teams
    |> List.map (fun t -> t.Team.skill)
    |> List.sort_rev Float.compare
    |> List.top_of_list t
    |> Pair.left
    |> List.fold_left (+.) 0.
  in

  Stats.add_sample u (best -. real)
;;

let combine (_ : t) u1 u2 = Stats.combine u1 u2;;

let score (_ : t) u = Stats.mean u, Stats.stderr u;;