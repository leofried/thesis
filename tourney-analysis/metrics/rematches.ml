open! Util
open! Std
open! Struct
open! Schemes
open! Engine

type t = Stats.t [@@deriving sexp];;

let kind = "rematches";;

let empty (_ : Specs.t) : t = Stats.empty 1;;

let fold (t : t) (specs : Specs.t) (scheme : Scheme.t) : t =
  let teams = Team.create_n specs (Scheme.number_of_teams scheme) in
  let _ = Scheme.run scheme (Team.play_game specs) teams in

  teams
  |> List.map (fun t -> t.Team.opps)
  |> List.flatten
  |> List.map Pair.right
  |> List.map (Fun.flip (-) 1)
  |> List.fold_left (+) 0
  |> Fun.flip (/) 2
  |> float_of_int
  |> List.item
  |> Fun.flip Stats.add_sample t
;;

let combine = Stats.combine;;

let score (t : t) _ = 
  0. -. List.hd (Stats.means t) /. 100.,
  0. -. List.hd (Stats.stderrs t) /. 100.,
  Stats.samples t
;;