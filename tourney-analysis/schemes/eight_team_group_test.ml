open! Util
open! Std
open! Struct

type t =
  | WorldCup
  | Jumble
[@@deriving sexp];;

let kind = "eight_team_group_test";;

let number_of_teams _ = 8;;

let run t specs teams = match teams with
  | [t1; t2; t3; t4; t5; t6; t7; t8] -> (
    let a1, a2 = Team.play_game specs ~is_bracket:false t1 t2 in
    let b1, b2 = Team.play_game specs ~is_bracket:false t3 t4 in
    let c1, c2 = Team.play_game specs ~is_bracket:false t5 t6 in
    let d1, d2 = Team.play_game specs ~is_bracket:false t7 t8 in

    let teams = 
      match t with
      | WorldCup -> [a1; b1; c1; d1; c2; d2; a2; b2]
      | Jumble -> [a1; b1; c1; d1; b2; d2; a2; c2]
    in
    Proper.run [8;0;0;0] specs teams
  )
  | _ -> assert false
  ;;