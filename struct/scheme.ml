open Util;;
open Infix;;

type t = 
  | Round_robin
  | Bracket of int list
  | Pools of int * t
  | Offset of int * t
  | Chain of t list
;;

let rec number_of_teams = function
  | Round_robin -> 1
  | Bracket lst -> Lists.fold (+) lst
  | Pools (_, scheme) -> number_of_teams scheme
  | Offset (n, scheme) -> n + number_of_teams scheme
  | Chain lst -> Lists.fold max (List.map number_of_teams lst)
;;

let rec name = function
  | Round_robin -> "round_robin"
  | Bracket bracket -> Lists.to_string Int.to_string false bracket ^ "-bracket"
  | Pools (pool_count, scheme) -> string_of_int pool_count ^ "-pool " ^ name scheme
  | Offset (offset, scheme) -> "<" ^ string_of_int offset ^ ">-" ^ name scheme
  | Chain lst -> String.concat " -> " (List.map name lst)
;;

let rec run = function
  | Round_robin -> fun teams ->
    let teams_arr = Array.of_list teams in
    
    let n = List.length teams in
    let wins_arr = Array.init n (fun _ -> Array.make n 0) in
    for i = 0 to n - 1 do
      let t1 = teams_arr.(i) in
      for j = i + 1 to n - 1 do
        let t2 = teams_arr.(j) in
          if fst @@ Team.play_game false t1 t2 = t1 then
            wins_arr.(i).(j) <- 1
          else
            wins_arr.(j).(i) <- 1
      done
    done;

    let module IMap = Map.Make(Int) in

    let score_team (games : int array array) (teams : int list) m (t1 : int) =
      let points = List.fold_left (fun x t2 -> x + games.(t1).(t2)) 0 teams in
      let new_lst =
        match IMap.find_opt points m with
        | None -> [t1]
        | Some lst -> t1 :: lst in
      IMap.add points new_lst m
    in

    let tiebreak (lst : int list) : int list =
      let nd = List.map (fun c -> (Random.bits (), c)) lst in
      let sond = List.sort compare nd in
      List.map snd sond
    in

    let rec rank_teams (teams : Team.t list) (indicies : int list): int list =
      let scores = List.fold_left (score_team wins_arr indicies) IMap.empty indicies in
      match IMap.cardinal scores with
      | 1 -> tiebreak indicies
      | _ ->
        let levels = snd @@ List.split @@ IMap.bindings @@ scores in
        List.fold_left (fun ranks level -> (rank_teams teams level) @ ranks) [] levels
    in
    
    List.map (Array.get teams_arr) (rank_teams teams (List.init (List.length teams) Fun.id))


  | Bracket bracket ->
    Lists.verify_base_two_sum bracket;

    let rec run_reversed (teams : Team.t list) = function
    | [] -> assert false
    | [_] -> teams
    | hd :: md :: tl ->
      let n = hd / 2 in
      let dead, alive =
        teams
        |> Lists.top_of_list_rev n
        |> Tuple.map_right (Lists.top_of_list n)
        |> Tuple.associate_left
        |> Tuple.map_left (
          Tuple.uncurry List.combine
          >> List.map (Tuple.uncurry (Team.play_game true))
          >> List.split
          >> Tuple.commute
        )
        |> Tuple.associate_right
        |> Tuple.map_right (Tuple.uncurry List.append)
      in
      (run_reversed alive ((md + n) :: tl)) @ dead

    in
    Lists.top_of_list (number_of_teams (Bracket bracket))
    >> Tuple.map_left (
      List.rev
      >> Fun.flip run_reversed bracket
    )
    >> Tuple.uncurry List.append


  | Pools (pool_count, scheme) ->
    let rec make_pots (teams : Team.t list) : Team.t list list =
      if List.length teams <= pool_count then [teams] else
        let pot, ts = Lists.top_of_list pool_count teams in
        pot :: make_pots ts
    in

    let rec reorient f = function
      | [] -> []
      | pots ->
        pots
        |> List.map List.tl
        |> List.filter ((<>) [])
        |> reorient f
        |> f (List.map List.hd pots)
    in

    make_pots
    >> reorient List.cons (*make pools*)
    >> List.map (run scheme)
    >> reorient List.append (*rank*)


  | Offset (n, scheme) ->
    Lists.top_of_list n
    >> Tuple.map_right (run scheme)
    >> Tuple.uncurry List.append


  | Chain lst ->
    Fun.flip (List.fold_left (Fun.flip run)) lst
;;

let max_games scheme = 
  Team.make_n
  >> run scheme
  >> List.map (fun t -> t.Team.games)
  >> Lists.fold max
;;