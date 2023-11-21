open! Util
open! Std
open! Schemes
open! Engine

type t = Stats.t [@@deriving sexp];;

let kind = "decay";;

let empty (specs : Specs.t) : t = Stats.empty specs.number_of_teams;;

let generate (specs : Specs.t) (schemes : Scheme.t list) =
  let teams, play = Team.preconstruct specs in
  List.map (fun scheme -> Stats.single (
    List.combine_map (fun t r -> Team.skill t -. Team.skill r)
      (Team.sort teams)
      (Scheme.run scheme (play ()) teams)
  )) schemes
;;

let combine = Stats.combine;;

let score (t : t) prize = 
  let t =
    t
    |> Stats.weight prize
    |> Stats.collapse
  in
  List.hd (Stats.means t),
  List.hd (Stats.stderrs t),
  Stats.samples t
;;