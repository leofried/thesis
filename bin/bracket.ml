open Util;;
(*
let rec sum (int_list : int list) : int =
  match int_list with
  | [] -> 0;
  | hd :: tl ->
    let s = sum tl in
    if s mod 2 = 1 then
      Throw.error()
    else
      hd + s / 2
;;

let check (int_list : int list) : unit =
  if sum int_list <> 1 then Throw.error()
;;
*)

let rec convert (int_list : int list) (i : int) (arr : int Tree.t array) : unit =
  match int_list with
  | [1] -> ()
  | [] | [_] -> Throw.error ()
  | 0 :: tl -> convert tl i arr
  | x :: _ when x < 0 -> Throw.error ()
  | a :: b :: tl -> 
    let j = i - a + 1 in
    arr.(j) <- Branch (arr.(j), arr.(i));
    convert (a - 2 :: b + 1 :: tl) (i - 1) arr
;;

let build_tree (int_list : int list) : int Tree.t =
  let n = Util.Int_list.sum_one_list int_list in
  let arr = Array.init n (fun i -> Tree.Leaf i) in
  convert int_list (n - 1) arr;
  arr.(0)
;;

let rec run_bracket (tree : int Tree.t) (teams : Team.t list) : Team.t list =
  match tree with
  | Leaf i -> [List.nth teams i]
  | Branch (i, j) ->
    let top = run_bracket i teams in
    let bot = run_bracket j teams in
    let winner, loser = Team.play_game (List.hd top) (List.hd bot) in
    winner :: loser :: (List.tl top) @ (List.tl bot)
;;

let make (bracket : int list) : Scheme.t =
  let name = Int_list.to_string Int.to_string bracket in
  let n = Int_list.sum_one_list bracket in
  Scheme.make_scheme name n (run_bracket (build_tree bracket))
;;


let rec bracket_children (bracket : int list) : int list list =
  match bracket with
  | [] -> []
  | hd :: tl -> 
    let lst = List.map (fun lst -> 0 :: Int_list.sum_two_lists [hd] lst) (bracket_children tl) in
    if hd = 0 then lst else (2 :: (hd - 1) :: tl) :: lst
;;

let rec get_all_brackets (number_of_teams : int) : int list list =
  match number_of_teams with
  | 1 -> [[1]]
  | n ->
    get_all_brackets (n-1)
    |> List.map bracket_children
    |> List.flatten
    |> List.sort_uniq (List.compare Int.compare)
