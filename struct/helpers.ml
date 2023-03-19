open Util;;

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

let round teams =
  let n = List.length teams / 2 in
  teams
  |> Lists.top_of_list_rev n
  |> Tuple.uncurry List.combine
  |> List.map (Tuple.uncurry (Team.play_game true))
  |> List.split
;;

let bracket bracket teams =
  Lists.verify_base_two_sum bracket;

  let rec run_reversed teams = function
    | [] -> assert false
    | [_] -> teams
    | hd :: md :: tl ->
      let players, byes = Lists.top_of_list hd teams in
      let winners, losers = round players in
      run_reversed (winners @ byes) (hd / 2 + md :: tl) @ losers
  in

  let players, dead = Lists.top_of_list (Lists.fold (+) bracket) teams in
  let order = run_reversed (List.rev players) bracket in
  order @ dead
;;