open! Util
open! Std
open! Struct

type t = int [@@deriving sexp];;

let kind = "round_robin";;

let number_of_teams = Fun.id;;

let run n specs teams =
  let teams_arr = Array.of_list teams in
  assert (List.length teams = n);

  let wins_arr = Array.init n (fun _ -> Array.make n 0) in
  for i = 0 to n - 1 do
    let t1 = teams_arr.(i) in
    for j = i + 1 to n - 1 do
      let t2 = teams_arr.(j) in
        if fst @@ Team.play_game specs ~is_bracket:false t1 t2 = t1 then
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

  (* think about tiebreakers *)
  let rec rank_teams (teams : Team.t list) (indicies : int list) : int list =
    let scores = List.fold_left (score_team wins_arr indicies) IMap.empty indicies in
    match IMap.cardinal scores with
    | 1 -> Random.shuffle indicies
    | _ ->
      let levels = snd @@ List.split @@ IMap.bindings @@ scores in
      List.fold_left (fun ranks level -> (rank_teams teams level) @ ranks) [] levels
  in
  
  [List.map (Array.get teams_arr) (rank_teams teams (List.create (List.length teams)))]
;;