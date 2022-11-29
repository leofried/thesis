open Util;;

let rec make_pots (pool_count : int) (teams : Team.t list) : Team.t list list =
  if List.length teams <= pool_count then [teams] else
    let pot, ts = Lists.top_of_list teams pool_count in
    Rand.shuffle pot :: make_pots pool_count ts
;;

let rec make_pools (pool_count : int) (pots : Team.t list list) : Team.t list list =
  match pots with
  | [] -> []
  | _ ->
    pots
    |> List.map List.tl
    |> List.filter Lists.is_not_empty
    |> make_pools pool_count
    |> List.cons (List.map List.hd pots)
  ;;

let rec make_seeds (pools : Team.t list list) : Team.t list =
  match pools with
  | [] -> []
  | _ ->
    pools
    |> List.map List.tl
    |> List.filter Lists.is_not_empty
    |> make_seeds
    |> List.append (List.map List.hd pools)
;;

let run_pool (pool : Team.t list) : Team.t list =
  Scheme.run (Round_robin.make (List.length pool)) pool
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

let make (number_of_teams : int) ?(pool_count : int = 1) (bracket : Scheme.t) : Scheme.t =
  Scheme.make_scheme
    (Int.to_string pool_count ^ " pool format breaking to a " ^ (Scheme.to_string bracket))
    number_of_teams
    (run_pool_to_bracket pool_count bracket)
;;