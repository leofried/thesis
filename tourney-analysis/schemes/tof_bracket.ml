open! Util;;
open! Std;;

type t =
  | Three
  | Balanced
  | Ladder
[@@deriving sexp];;

let kind = "tof_bracket";;

let number_of_teams = function
  | Three -> 3
  | Balanced | Ladder -> 4
;;

let run t ~luck teams = match t with
  | Three ->
    let t1 = List.nth teams 0 in
    let t2 = List.nth teams 1 in
    let t3 = List.nth teams 2 in

    let w1, l1 = Team.play_game ~luck ~is_bracket:true t2 t3 in
    let w2, l2 = Team.play_game ~luck ~is_bracket:true t1 w1 in

    [w2; l2; l1]

  | Balanced ->
    let t1 = List.nth teams 0 in
    let t2 = List.nth teams 1 in
    let t3 = List.nth teams 2 in
    let t4 = List.nth teams 3 in

    let w1, l1 = Team.play_game ~luck ~is_bracket:true t1 t4 in
    let w2, l2 = Team.play_game ~luck ~is_bracket:true t2 t3 in
    let ww, wl = Team.play_game ~luck ~is_bracket:true w1 w2 in

    [ww; wl; l1; l2]

  | Ladder -> 
    let t1 = List.nth teams 0 in
    let t2 = List.nth teams 1 in
    let t3 = List.nth teams 2 in
    let t4 = List.nth teams 3 in

    let w1, l1 = Team.play_game ~luck ~is_bracket:true t3 t4 in
    let w2, l2 = Team.play_game ~luck ~is_bracket:true t2 w1 in
    let w3, l3 = Team.play_game ~luck ~is_bracket:true t1 w2 in

    [w3; l3; l2; l1]
;;