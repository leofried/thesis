open Util;;

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

let rec convert (rev_int_list : int list) (i : int) (arr : int Tree.t array) : unit =
  match rev_int_list with
  | [1] -> ()
  | [] | [_] -> Throw.error ()
  | 0 :: tl -> convert tl i arr
  | a :: b :: tl -> 
    let j = i - a + 1 in
    arr.(j) <- Branch (arr.(j), arr.(i));
    convert (a - 2 :: b + 1 :: tl) (i - 1) arr
;;


let build_tree (int_list : int list) : int Tree.t =
  check int_list;
  let n = Util.Int_list.sum_one_list int_list in
  let arr = Array.init n (fun i -> Tree.Leaf i) in
  convert (List.rev int_list) (n - 1) arr;
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
  let name = Int_list.to_string bracket in
  let n = Int_list.sum_one_list bracket in
  Scheme.make_scheme name n (run_bracket (build_tree bracket))
;;



(*
let rec bracket_to_int_list (bracket : t) : int list =
  match bracket with
  | Line _ -> [1]
  | Game (i, j) -> 0 :: Util.sum_two_lists (bracket_to_int_list i) (bracket_to_int_list j)
;;
*)