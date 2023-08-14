open! Util
open! Std
open! Struct

type mode =
  | NoGame
  | Losers
  | WinnersOne
  | WinnersTwo
[@@deriving sexp];;

type t = Bracket.t * mode [@@deriving sexp];;


let kind = "eight_team_double_elimination";;

let number_of_teams _ = 8;;

let run (bracket, mode) specs teams =
  match Bracket.run bracket specs teams with
  | [t1; t2; tb1; tb2; ta1; ta2; ta3; ta4] ->
    let w1, l1 = Team.play_game ~is_bracket:true specs ta1 ta2 in
    let w2, l2 = Team.play_game ~is_bracket:true specs ta3 ta4 in

    let w3, l3 = Team.play_game ~is_bracket:true specs w1 tb2 in
    let w4, l4 = Team.play_game ~is_bracket:true specs w2 tb1 in

    let w5, l5 = Team.play_game ~is_bracket:true specs w3 w4 in


    let losers =  [l5; l4; l3; l2; l1] in

    if mode = NoGame then
      t1 :: t2 :: w5 :: losers
    else
      let w6, l6 = Team.play_game ~is_bracket:true specs t2 w5 in
      let losers = l6 :: losers in
      if mode = Losers then
        t1 :: w6 :: losers
      else
        let w7, l7 = Team.play_game ~is_bracket:true specs t1 w6 in
        if mode = WinnersOne || w7 = t1 then
          w7 :: l7 :: losers
        else
          let w8, l8 = Team.play_game ~is_bracket:true specs t1 w6 in
          w8 :: l8 :: losers

  | _ -> assert false
;;


let get_all opts : t list =
  Branch (
    Branch (
      Branch (Leaf (), Leaf ()),
      Branch (Leaf (), Leaf ())
    ),
    Branch (
      Branch (Leaf (), Leaf ()),
      Branch (Leaf (), Leaf ())
    )
  )
  |> Bracket.get_all_from_shape
  |> List.map Pair.pair
  |> List.map (Fun.flip List.map opts)
  |> List.flatten
;;



