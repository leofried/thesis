open Util;;

let kind = "pool_play";;

let rec make_pots (pool_count : int) (teams : Team.t list) : Team.t list list =
  if List.length teams <= pool_count then [teams] else
    let pot, ts = Lists.top_of_list teams pool_count in
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
  (Round_robin.make (List.length pool)).run pool
;;

let run_bracket (bracket : Scheme.t) (teams : Team.t list) : Team.t list =
  let teams_to_bracket = bracket.number_of_teams in
  let top = List.filteri (fun i _ -> i < teams_to_bracket) teams in
  let bottom = List.filteri (fun i _ -> i >= teams_to_bracket) teams in
  let ranks = bracket.run top in
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


let make ~(number_of_teams : int) ~(pool_count : int) (bracket : int list) : Scheme.t =
  let bracket_scheme = Bracket.make bracket in
  {
    name = Int.to_string number_of_teams ^ " team " ^ Int.to_string pool_count ^ " pool format breaking to a " ^ bracket_scheme.name;
    number_of_teams;
    max_games = Math.divide_up number_of_teams pool_count + bracket_scheme.max_games - 1;
    is_fair = number_of_teams mod pool_count = 0 && Bracket.is_fair bracket pool_count;
    run = run_pool_to_bracket pool_count bracket_scheme;
    json = `Assoc [(Scheme.kind_key, `String kind); ("number_of_teams", `Int number_of_teams); ("pool_count", `Int pool_count); ("bracket", bracket_scheme.json); ]
  }
;;

(*has to now about bracket internals*)
let make_from_json (json : Json.t) : Scheme.t = make
  ~number_of_teams: (Json.rip_int "number_of_teams" json)
  ~pool_count: (Json.rip_int "pool_count" json)
 (Json.rip_list "bracket" (Json.member "bracket" json))
;;

let get_all_pools ~number_of_teams ~pool_counts ~max_games : Scheme.t list =
  let all_brackets = List.flatten (Bracket.get_all_brackets number_of_teams) in
  pool_counts
  |> List.map (fun pool_count -> List.map (make ~number_of_teams ~pool_count) all_brackets)
  |> List.flatten
  |> List.filter (fun (s : Scheme.t) -> s.max_games <= max_games)
;;