let play_games (teams : Team.t list) (cycles : int) : int array array =
  let n = List.length teams in
  let arr = Array.init n (fun _ -> Array.make n 0) in
  for i = 0 to n - 1 do
    let t1 = List.nth teams i in
    for j = i + 1 to n - 1 do
      let t2 = List.nth teams j in
      for _ = 1 to cycles do
        if fst @@ Team.play_game t1 t2 = t1 then
          arr.(i).(j) <- (arr.(i).(j)) + 1
        else
          arr.(j).(i) <- (arr.(j).(i)) + 1
      done
    done
  done;
  arr
;;

module IMap = Map.Make(Int);;
type imap = int list IMap.t;;

let score_team (games : int array array) (teams : int list) (m : imap) (t1 : int) : imap =
  let points = List.fold_left (fun x t2 -> x + games.(t1).(t2)) 0 teams in
  let new_lst =
    match IMap.find_opt points m with
    | None -> [t1]
    | Some lst -> t1 :: lst in
  IMap.add points new_lst m
;;

let tiebreak (lst : int list) : int list =
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
  let sond = List.sort compare nd in
  List.map snd sond
;;

let rec rank_teams (teams : Team.t list) (indicies : int list) (games : int array array) : int list =
  let scores = List.fold_left (score_team games indicies) IMap.empty indicies in
  match IMap.cardinal scores with
  | 1 -> tiebreak indicies
  | _ ->
    let levels = snd @@ List.split @@ IMap.bindings @@ scores in
    List.fold_left (fun ranks level -> (rank_teams teams level games) @ ranks) [] levels
;;

let run_round_robin (cycles : int) (teams : Team.t list) : Team.t list = 
  play_games teams cycles
  |> rank_teams teams (List.init (List.length teams) Fun.id)
  |> List.map (List.nth teams)
;;

let make ?(cycles = 1) (number_of_teams : int) : Scheme.t =
  Scheme.make_scheme ((Int.to_string cycles) ^ "-Round Robin") number_of_teams (number_of_teams - 1) (fun _ -> true) (run_round_robin cycles)
;;