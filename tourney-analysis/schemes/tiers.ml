open! Util
open! Std
open! Struct

type s =
  | Strongly (*must be completely identical*)
  | Weakly   (*must be completely identical up until chalked loss*)
  | Byely    (*must have the same number of byes*)
  | None     (*no restrictions*)
;;

type t = int list;;





(* 
let get_all
  ?(max_games = None)
  ?(target_sum = 1) 
  ?(require_games = false)
  tiers
: Proper.t list =  
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
;; *)