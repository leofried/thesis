open! Util
open! Std
open! Struct
open! Schemes

type t = int list list [@@deriving sexp];;

let kind = "disorder";;

let empty (specs : Specs.t) : t =
  specs.number_of_teams 
  |> List.create 
  |> List.map (Fun.const (
    specs.number_of_teams 
    |> List.create 
    |> List.map (Fun.const 0)
  ))
;;

let fold (t : t) (specs : Specs.t) (scheme : Scheme.t) : t =
  let target = Team.create specs () in
  let teams = Team.create_n specs (Scheme.number_of_teams scheme - 1) in

  List.mapi (fun i ->
    List.on_loc
      (
        teams
        |> List.insert i target
        |> Scheme.run scheme specs
        |> List.index target
      )
      ((+) 1)
  ) t
;;


(*list.map combine*)
let combine (t1 : t) (t2 : t) : t=
  List.combine t1 t2 
  |> List.map (fun (l1, l2) -> 
    List.combine l1 l2
    |> List.map (Pair.uncurry (+))
  )
;;

let score (t : t) prize =
  let scores = 
    List.map (fun lst -> 
      List.combine lst prize
      |> List.map (Pair.uncurry Math.mul_int_float)
      |> List.fold_left (+.) 0.
    ) t
  in

  let count = List.fold_left (+) 0 (List.hd t) in

  Math.divide_float_int
    (List.fold_downstream (fun score hd tl ->
      List.fold_left (fun score tl ->
        score +. if hd < tl then tl -. hd else 0.
      ) score tl
    ) 0. scores)
    count
  , 0., count
;;