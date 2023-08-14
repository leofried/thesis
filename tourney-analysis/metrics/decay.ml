open! Util
open! Std
open! Struct
open! Schemes


(*list.combine map*)
type t = Stats.t [@@deriving sexp];;

let kind = "decay";;

let empty (specs : Specs.t) : t = Stats.empty specs.number_of_teams;;

let fold (t : t) (specs : Specs.t) (scheme : Scheme.t) : t =
  let teams = Team.create_n specs (Scheme.number_of_teams scheme) in
 
  Stats.add_sample (
    List.combine_map (fun t r -> Team.skill t -. Team.skill r)
      (Team.sort teams)
      (Scheme.run scheme specs teams)
  ) t
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