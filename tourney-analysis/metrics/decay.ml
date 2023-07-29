open! Util
open! Std
open! Struct
open! Schemes

type t = Stats.t list [@@deriving sexp];;

let kind = "decay";;

let empty (specs : Specs.t) : t = List.map (Fun.const Stats.empty) (List.create (1 + specs.number_of_teams));;

let fold (t : t) (specs:Specs.t) (scheme : Scheme.t) =
  let teams = Team.create ~fidel:specs.fidel ~n:(Scheme.number_of_teams scheme) in
  let results = Scheme.run scheme ~luck:specs.luck teams in

  let top m =
    List.top_of_list m
    >> Pair.left
    >> List.map Team.skill
    >> List.fold_left (+.) 0.
  in

  List.mapi (fun i stats -> Stats.add_sample stats (top i (Team.sort teams) -. top i results)) t
;;

let combine (t1 : t) (t2 : t) : t = 
  List.combine t1 t2
  |> List.map (Pair.uncurry Stats.combine)
;;

let score (t : t) = 
  let stats = List.nth t 1 in
  Stats.mean stats,
  Stats.stderr stats,
  Stats.count stats
;;