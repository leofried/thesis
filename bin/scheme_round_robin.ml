open Util;;

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

let rec rank_teams (teams : Team.t list) (games : int array array) (indicies : int list) : Team.t list =
  let scores = List.fold_left (score_team games indicies) IMap.empty indicies in
  match IMap.cardinal scores with
  | 1 -> Rand.shuffle (List.map (List.nth teams) indicies)
  | _ ->
    let levels = snd @@ List.split @@ IMap.bindings @@ scores in
    List.fold_left (fun ranks level -> (rank_teams teams games level) @ ranks) [] levels
;;

let run_round_robin (cycles : int) (teams : Team.t list) : Team.t list = 
  let games = play_games teams cycles in
  rank_teams teams games (List.init (List.length teams) Fun.id)
;;

let make ?(cycles = 1) (number_of_teams : int) : Scheme.t =
  Scheme.make_scheme ((Int.to_string cycles) ^ "-Round Robin") number_of_teams (run_round_robin cycles)
;;