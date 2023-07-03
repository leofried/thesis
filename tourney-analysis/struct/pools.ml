open Util;;
open Infix;;
open Engine;;

let round_robin teams =
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

  let rec rank_teams (teams : Team.t list) (indicies : int list): int list =
    let scores = List.fold_left (score_team wins_arr indicies) IMap.empty indicies in
    match IMap.cardinal scores with
    | 1 -> Rand.shuffle indicies
    | _ ->
      let levels = snd @@ List.split @@ IMap.bindings @@ scores in
      List.fold_left (fun ranks level -> (rank_teams teams level) @ ranks) [] levels
  in
  
  List.map (Array.get teams_arr) (rank_teams teams (List.init (List.length teams) Fun.id))
;;

let run pool_count =
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
  >> List.map round_robin
(*  >> reorient List.append rank*)
;;