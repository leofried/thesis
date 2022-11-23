let rec make_pots (pool_count : int) (teams : Team.t list) : Team.t list list =
  if List.length teams <= pool_count then [teams] else
    let pot, ts = Util.apply (fun (pot, ts) -> List.hd ts :: pot, List.tl ts) pool_count ([], teams) in
    Util.shuffle pot :: make_pots pool_count ts
;;

let rec make_pools (pool_count : int) (pots : Team.t list list) : Team.t list list =
  match pots with
  | [] -> []
  | _ ->
    pots
    |> List.map List.tl
    |> List.filter Util.is_not_empty
    |> make_pools pool_count
    |> List.cons (List.map List.hd pots)
  ;;

let rec make_seeds (pools : Team.t list list) : Team.t list =
  match pools with
  | [] -> []
  | _ ->
    pools
    |> List.map List.tl
    |> List.filter Util.is_not_empty
    |> make_seeds
    |> List.append (Util.shuffle (List.map List.hd pools))
;;

let run_pool (pool : Team.t list) : Team.t list =
  Scheme.run (Scheme_round_robin.make (List.length pool)) pool
;;

let run_bracket (teams_to_bracket : int) (teams : Team.t list) : Team.t list =
  let bracket = List.filteri (fun i _ -> i < teams_to_bracket) teams in
  let bottom = List.filteri (fun i _ -> i >= teams_to_bracket) teams in
  let ranks = Scheme.run (Scheme_simple_bracket.make (teams_to_bracket)) bracket in
  ranks @ bottom
;;

let run_pool_to_bracket ~(pool_count : int) ~(teams_to_bracket : int) (teams : Team.t list) : Team.t list =
  teams
  |> make_pots pool_count
  |> make_pools pool_count
  |> List.map run_pool
  |> make_seeds
  |> run_bracket teams_to_bracket
;;

let make ~(pool_count : int) ~(teams_to_bracket : int) (number_of_teams : int) : Scheme.t =
  Scheme.make_scheme
    (Int.to_string pool_count ^ " pool format breaking " ^ Int.to_string teams_to_bracket ^ " teams to bracket")
    number_of_teams
    (run_pool_to_bracket ~pool_count ~teams_to_bracket)
;;