open! Util
open! Std

type t = {
  number_of_teams : int;
  cycles : int;
 } [@@deriving sexp];;

let kind = "round_robin";;

let number_of_teams t = t.number_of_teams;;

let run t game teams =
  let n = t.number_of_teams in
  let teams_arr = Array.of_list teams in
  assert (List.length teams = n);

  let wins_arr = Array.init n (fun _ -> Array.make n 0) in
  for _ = 1 to t.cycles do
    for i = 0 to n - 1 do
      let t1 = teams_arr.(i) in
      for j = i + 1 to n - 1 do
        let t2 = teams_arr.(j) in
          if fst @@ game.Game.play t1 t2 = t1 then
            wins_arr.(i).(j) <- wins_arr.(i).(j) + 1
          else
            wins_arr.(j).(i) <- wins_arr.(j).(i) + 1
      done
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
  let rec rank_teams teams (indicies : int list) : int list =
    let scores = List.fold_left (score_team wins_arr indicies) IMap.empty indicies in
    match IMap.cardinal scores with
    | 1 -> 
      begin match game.tiebreaker with
        | Random -> List.shuffle indicies
        | WorstCase -> List.sort_by_rev (List.nth teams) game.compare indicies
      end
    | _ -> 
      scores
      |> IMap.bindings
      |> List.split
      |> Pair.right
      |> List.map (rank_teams teams)
      |> List.rev
      |> List.flatten
  in
  [List.map (Array.get teams_arr) (rank_teams teams (List.create (List.length teams)))]
;;