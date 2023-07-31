open! Util
open! Std
open! Struct
open! Schemes

type t = Stats.t list [@@deriving sexp];;

let kind = "decay";;

let empty (specs : Specs.t) : t = List.map (Fun.const Stats.empty) (List.create specs.number_of_teams);;

let fold (t : t) (specs:Specs.t) (scheme : Scheme.t) : t =
  let teams = Team.create_n specs (Scheme.number_of_teams scheme) |> Team.sort in
  let results = Scheme.run scheme specs teams in

  List.combine teams results
  |> List.combine t
  |> List.fold_left_map (fun diff (stats, (team, result)) ->
      let diff = diff +. (Team.skill team) -. (Team.skill result) in
      diff, Stats.add_sample stats diff
    ) 0.
  |> Pair.right
;;

let combine (t1 : t) (t2 : t) : t = 
  List.combine t1 t2
  |> List.map (Pair.uncurry Stats.combine)
;;

let score (t : t) = 
  let stats = List.nth t 0 in
  Stats.mean stats,
  Stats.stderr stats,
  Stats.count stats
;;