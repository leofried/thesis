open Util;;

let rec make_pots (pool_count : int) (teams : Team.t list) : Team.t list list =
  if List.length teams <= pool_count then [teams] else
    let pot, ts = Lists.top_of_list pool_count teams in
    pot :: make_pots pool_count ts
;;

let rec make_pools (pool_count : int) (pots : Team.t list list) : Team.t list list =
  match pots with
  | [] -> []
  | _ ->
    pots
    |> List.map List.tl
    |> List.filter ((<>) [])
    |> make_pools pool_count
    |> List.cons (List.map List.hd pots)
  ;;

let rec make_seeds (pools : Team.t list list) : Team.t list =
  match pools with
  | [] -> []
  | _ ->
    pools
    |> List.map List.tl
    |> List.filter ((<>) [])
    |> make_seeds
    |> List.append (List.map List.hd pools)
;;

let run_pool (pool : Team.t list) : Team.t list = 
  Round_robin.run (List.length pool, 1) pool
;;

let run_bracket (bracket : Scheme.t) (teams : Team.t list) : Team.t list =
  let teams_to_bracket = Scheme.number_of_teams bracket in
  let top = List.filteri (fun i _ -> i < teams_to_bracket) teams in
  let bottom = List.filteri (fun i _ -> i >= teams_to_bracket) teams in
  let ranks = Scheme.run bracket top in
  ranks @ bottom
;;

let run_pool_to_bracket (pool_count : int) (bracket : Scheme.t) (teams : Team.t list) : Team.t list =
  teams
  |> make_pots pool_count
  |> make_pools pool_count
  |> List.map run_pool
  |> make_seeds
  |> run_bracket bracket
;;

(*let max_games_helper ~number_of_teams ~pool_count ~bracket = Math.divide_up number_of_teams pool_count + Bracket.max_games bracket - 1;;*)



type argument = int * int * int list [@@deriving yojson];;

let number_of_teams (number_of_teams, _, _ : argument) = number_of_teams;;

let name (number_of_teams, pool_count, bracket : argument) = Int.to_string number_of_teams ^ " team " ^ Int.to_string pool_count ^ " pool format breaking to a " ^ Bracket.name bracket;;

(*let max_games (number_of_teams, pool_count, bracket : argument) = max_games_helper ~number_of_teams ~pool_count ~bracket*)

let is_fair (_ : argument) = assert false;;
  (*number_of_teams mod pool_count = 0 && Bracket.is_fair_helper bracket pool_count;;*)

let run (_, pool_count, bracket : argument) = run_pool_to_bracket pool_count (Scheme.Format ((module Bracket), bracket));;

let kind = "pool_play";;

(* do we want to restrict to pool count numbers?
let get_all ~(number_of_teams : int) ~(max_games : int) : argument list =
  let all_brackets = List.flatten (Bracket.get_all_brackets number_of_teams) in
  let _  = max_games in
  
  List.init (Math.divide_up number_of_teams 2) ((+) 1)
  |> List.cons number_of_teams
  |> List.map (fun pool_count -> List.map (fun (bracket : int list) -> number_of_teams, pool_count, bracket) all_brackets)
  |> List.flatten
(*  |> List.filter (fun (number_of_teams, pool_count, bracket) -> max_games_helper ~number_of_teams ~pool_count ~bracket <= max_games)*)
;;*)