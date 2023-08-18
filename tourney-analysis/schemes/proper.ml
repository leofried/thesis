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


let get_all
  ?(max_games = None)
  ?(target_sum = 1) 
  ?(require_games = false)
  tiers 
: t list =  
  let rec f max_games target_sum curr tiers : t list =
    if target_sum = 0 then [[curr]] else
    if target_sum < 0 then [] else
    if Option.fold ((>=) 0) max_games false then [] else 
      let hds = List.map (List.cons curr) (f (Option.map (Fun.flip (-) 1) max_games) (target_sum * 2) 0 tiers) in
      if tiers = [] then hds else 
        let next, new_tiers = List.pop tiers in
        hds @ f max_games (target_sum - next) (curr + next) new_tiers
          
  in List.map List.rev (
    if require_games then (List.map (List.cons 0) (f (Option.map (Fun.flip (-) 1) max_games) (target_sum * 2) 0 tiers))
    else f max_games target_sum 0 tiers
  )
;;