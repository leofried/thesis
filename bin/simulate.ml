open Util;;

let rec get_best_team_skill (teams : Team.t list) : float =
  match teams with
  | [] -> System.error ()
  | [t] -> Team.get_skill t
  | hd :: tl -> Float.max (Team.get_skill hd) (get_best_team_skill tl)
;;

let rec run_sims (scheme : Scheme.t) (iters_left : int) (decays: float list) (seed_wins : int array) : float list =
  if iters_left = 0 then decays else
  let teams = List.init (scheme.number_of_teams) (fun _ -> Team.make ()) in
  let winner = List.hd @@ scheme.run teams in
  let decay = get_best_team_skill teams -. Team.get_skill winner in
  Math.inc_array seed_wins (Lists.find winner teams);
  run_sims scheme (iters_left - 1) (decay :: decays) seed_wins
;;

let get_data ~(iters : int) (scheme : Scheme.t) : Data.t =
  let seed_wins = Array.make (scheme.number_of_teams) 0 in
  let sims = run_sims scheme iters [] seed_wins in
  {
    iters;
    decay = Stats.mean sims;
    margin = Stats.stderr sims;
    seed_wins = Array.to_list seed_wins;
    is_fair = scheme.is_fair (scheme.number_of_teams);
    max_games = scheme.max_games
  }
;;

let combine_data (new_data : Data.t) (old_data : Data.t) : Data.t =
  {
    iters = old_data.iters + new_data.iters;
    decay = Stats.mean_two (old_data.iters, old_data.decay) (new_data.iters, new_data.decay);
    margin = Stats.stderr_two (old_data.iters, old_data.decay, old_data.margin) (new_data.iters, new_data.decay, new_data.margin);
    seed_wins = Lists.sum_two_lists old_data.seed_wins new_data.seed_wins;
    is_fair = new_data.is_fair;
    max_games = new_data.max_games
  }

let sim_scheme ~(luck : float) ~(iters : int) (scheme : Scheme.t) : unit =
  if iters < 2 then System.error ();
  Team.set_luck luck;

  let number_of_teams = scheme.number_of_teams in
  let name = scheme.name in

  let new_data = get_data ~iters scheme in
  let read_json = Json.read ~luck ~number_of_teams in

  let combined_data =
    if Json.has_key name read_json then
      combine_data new_data (Data.json_to_data (Json.member name read_json))
    else
      new_data
  in

  read_json
  |> Json.set_key name (Data.data_to_json combined_data)
  |> Json.write ~luck ~number_of_teams
;;

let sim_schemes ~(luck : float) ~(iters : int) (schemes : Scheme.t list) : unit =
  let n = ref 0 in
  List.iter (fun scheme ->
    Math.inc_ref n;
    print_endline (Int.to_string !n);
    sim_scheme ~luck ~iters scheme
  ) schemes
;;

let rec p_helper (lst : (string * float * float) list) ((name, data) : string * Data.t ) : string * float = 
  let imb = Data.calculate_imbalance data true in
  match lst with
  | [] -> System.error ()
  | (s, _, _) :: _ when s = name -> name, 0.
  | (_, c, d) :: _ when d <= imb -> name, 1. /. Float.pow 2. ((data.decay -. c) /. data.margin)
  | _ :: tl -> p_helper tl (name, data)
;;

let sim_smart  ~(luck : float) ~(number_of_teams : int) ~(max_games : int) ~(iters : int) : unit =
  let pareto_list = Report.pareto_list ~luck ~number_of_teams ~max_games in

  begin if Random.bool () then
     (List.map (fun (name, _, _) -> name, 1.) pareto_list)
  else
    List.map (p_helper pareto_list) (Data.get_data_list ~luck ~number_of_teams ~max_games)
  end
  |> Stats.sample
  |> Data.read_scheme_from_name
  |> sim_scheme ~luck ~iters
;;