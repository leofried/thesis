let play_games (teams : Team.t list) (cycles : int) : int array array =
  let teams_arr = Array.of_list teams in

  let n = List.length teams in
  let wins_arr = Array.init n (fun _ -> Array.make n 0) in
  for i = 0 to n - 1 do
    let t1 = teams_arr.(i) in
    for j = i + 1 to n - 1 do
      let t2 = teams_arr.(j) in
      for _ = 1 to cycles do
        if fst @@ Team.play_game t1 t2 = t1 then
          wins_arr.(i).(j) <- (wins_arr.(i).(j)) + 1
        else
          wins_arr.(j).(i) <- (wins_arr.(j).(i)) + 1
      done
    done
  done;
  wins_arr
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



type argument = int * int;;

let name ((number_of_teams, cycles) : argument) = (Int.to_string number_of_teams ^ " team " ^ Int.to_string cycles ^ "-Round Robin");;

let number_of_teams ((number_of_teams, _) : argument) =  number_of_teams ;;

let max_games ((number_of_teams, cycles) : argument) = (number_of_teams - 1) * cycles;;

let is_fair ((_, _) : argument) = true;;

let run ((_, cycles) : argument) = run_round_robin cycles;;

let kind = "round_robin";;


(*
type one_argument = Param_specs.one_int * Param_specs.one_int;;

let params : (argument, one_argument) Param_specs.t = Param_specs.((Int "number_of_teams") ** (Int_def ("cycles", 1)));



let make ~(cycles : int) ~(number_of_teams : int) : Scheme.t =
  {
    name = Int.to_string number_of_teams ^ " team " ^ Int.to_string cycles ^ "-Round Robin";
    number_of_teams;
    max_games = number_of_teams - 1;
    is_fair = true;
    run = run_round_robin cycles;
  }
;;

let eliom : (int * int, Param_specs.one_int * Param_specs.one_int) Scheme.eliom_builder = 
  {
    name = "round_robin";
    params = Param_specs.((Int "number_of_teams") ** (Int_def ("cycles", 1)));
    make = fun (number_of_teams, cycles) -> make ~number_of_teams ~cycles;
  }
  *)