open Util;;
open Infix;;
open Engine;;

type t = int list [@@deriving yojson];;

let rec count_advance = function
  | [] -> invalid_arg "Bracket.count_advance"
  | [x] -> x
  | hd :: md :: tl ->
    if hd mod 2 = 1 then
      invalid_arg "Bracket.count_advance"
    else
      count_advance (md + hd / 2 :: tl)
;;

let run_round teams =
  let n = List.length teams / 2 in
  teams
  |> Lists.top_of_list_rev n
  |> Tuple.uncurry List.combine
  |> List.map (Tuple.uncurry (Team.play_game true))
  |> List.split
;;

let run_bracket bracket teams =
  let rec run_reversed teams = function
    | [] -> assert false
    | [_] -> teams
    | hd :: md :: tl ->
      let players, byes = Lists.top_of_list hd teams in
      let winners, losers = run_round players in
      run_reversed (winners @ byes) (hd / 2 + md :: tl) @ losers
  in

  let players, dead = Lists.top_of_list (Lists.fold (+) bracket) teams in
  let order = run_reversed (List.rev players) bracket in
  order @ dead
;;

let get_all_brackets ~(max_games : int) ~(target_sum : int) ~(tiers : Tiers.t) ~(require_games : bool)  : t list =
  let rec f (max_games : int) (target_sum : int) (curr: int) (tiers : Tiers.t) : t list =
    if target_sum = 0 then [[curr]] else
    if target_sum < 0 then [] else
    if max_games < 0 then [] else 
      let hds = List.map (List.cons curr) (f (max_games - 1) (target_sum * 2) 0 tiers) in
      if Tiers.is_empty tiers then hds else 
        let next, new_tiers = Tiers.pop tiers in
        hds @ f max_games (target_sum - next) (curr + next) new_tiers
          
  in List.map List.rev (
    if require_games then (List.map (List.cons 0) (f (max_games - 1) (target_sum * 2) 0 tiers))
    else f max_games target_sum 0 tiers
  )

let input_tiers : t -> Tiers.t = List.rev >>@ List.filter ((<>) 0);;

let output_tiers : t -> Tiers.t =
  let rec f = function
    | [] -> assert false
    | [n] -> [n]
    | hd :: md :: tl -> hd / 2 ::f (md + hd / 2 :: tl)
  in input_tiers >>@ f
;;