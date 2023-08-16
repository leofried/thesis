open! Util
open! Std
open! Struct

type t = int list [@@deriving sexp];;

let kind = "proper";;

let number_of_teams = List.fold_left (+) 0;;

let rec count_advance = function
  | [] -> invalid_arg "Bracket.count_advance"
  | [x] -> x
  | hd :: md :: tl ->
    if hd mod 2 = 1 then
      invalid_arg "Bracket.count_advance"
    else
      count_advance (md + hd / 2 :: tl)
;;

let build_brackets t =
  let count = count_advance t in
  let lst, i, c = 
    List.fold_right (fun b (lst, i, c) ->
      List.fold_left (fun lst j ->
        if i = 0 then
          Tree.Leaf (count - j) :: lst 
        else 
          List.map Tree.(map_tree (fun k -> if k = i - j then Branch (Leaf k, Leaf (i + j + 1)) else Leaf k)) lst
      ) lst (List.create c),
      i + c,
      2 * c - b
    )
    (List.on_loc (List.length t - 1) ((+) count) t)
    ([], 0, count)
  in
    assert (i = number_of_teams t);
    assert (c = 0);
    lst
;; 

let run t specs teams =
  build_brackets t
  |> List.map (fun bracket -> Bracket.run bracket specs teams)
  |> List.fold_left (List.combine_mismatched List.append) []
;;  