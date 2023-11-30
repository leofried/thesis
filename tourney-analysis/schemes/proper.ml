open! Util
open! Std

type t = int list [@@deriving sexp];;

let kind = "proper";;

let number_of_teams = List.fold_left (+) 0;;

let rec number_of_winners = function
  | [] -> 0
  | [x] -> x
  | hd :: md :: tl ->
    if hd mod 2 = 1 then
      invalid_arg "Bracket.number_of_winners"
    else
      number_of_winners (md + hd / 2 :: tl)
;;

let build_brackets t =
  let count = number_of_winners t in
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

let run t play teams =
  build_brackets t
  |> List.map (fun bracket -> Bracket.run bracket play teams)
  |> List.fold_left Bracket.combine_losers []
;;

let get_all 
  ?(max_target_sum = 1)
  ?(include_smaller = false)
  ?(include_trivial = true)
  ?(include_semitrivial = true)
  ?(respectfulness = Tier.Not)
  ?(max_games = Int.max_int)
  input_tiers
= 
  let rec f = function
    | 0 -> [[]]
    | n ->
        let lst = f (n-1) in
        lst
        |> List.hd
        |> List.map (fun lst ->
            List.length lst
            |> List.create
            |> List.filter_map (function
                | 0 -> Some (2 :: (List.hd lst - 1) :: (List.tl lst))
                | i -> match List.nth lst i with
                  | 0 -> None
                  | _ -> Some (
                      lst
                      |> List.on_loc (i - 1) ((+) 2)
                      |> List.on_loc i (Fun.flip (-) 1)
                    )
              )
          )
        |> List.flatten
        |> List.drop_dupes
        |> Bool.do_if (n <= max_target_sum) (List.cons [n])
        |> Fun.flip List.cons lst
  in
  input_tiers
  |> Tier.number_of_teams
  |> f
  |> (if include_smaller then List.flatten else List.hd)
  |> Bool.do_if_not include_trivial (List.filter (fun lst -> List.length lst <> 1))
  |> Bool.do_if_not include_semitrivial (List.filter (fun lst -> List.length lst = 1 || List.nth_one lst (List.length lst) = 0))
  |> List.filter_map (fun lst -> Pair.envelope lst (Tier.check_bracket respectfulness input_tiers lst))
  |> List.filter (fun (_, tiers) -> List.for_all (fun tier -> tier.Tier.games_played <= max_games) tiers)
;;