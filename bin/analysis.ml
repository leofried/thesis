open Util;;

type data = {
  iters : int;
  decay : float;
  margin : float;
  seed_wins : int list;
  is_fair : bool;
  max_games : int;
}

let json_to_data (json : Json.t) : data =
  {
    iters = Json.rip_int "iters" json;
    decay = Json.rip_float "decay" json;
    margin = Json.rip_float "margin" json;
    seed_wins = Json.rip_int_list "seed_wins" json;
    is_fair = Json.rip_bool "is_fair" json;
    max_games = Json.rip_int "max_games" json
  }
;;

let data_to_json (data : data) : Json.t =
  `Assoc [
    ("iters", `Int data.iters);
    ("decay", `Float data.decay);
    ("margin", `Float data.margin);
    ("seed_wins", `List (List.map (fun x -> `Int x) data.seed_wins));
    ("is_fair", `Bool data.is_fair);
    ("max_games", `Int data.max_games)
  ]
;;

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

let get_data ~(iters : int) (scheme : Scheme.t) : data =
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

let analyze_scheme ~(luck : float) ~(iters : int) (scheme : Scheme.t) : unit =
  if iters < 2 then System.error ();
  Team.set_luck luck;

  let number_of_teams = scheme.number_of_teams in
  let name = scheme.name in

  let new_data = get_data ~iters scheme in

  let old_json = Json.read ~luck ~number_of_teams in
  let combined_data = if Json.has_key name old_json then
    let old_data = json_to_data (Json.member name old_json) in
    {
      iters = old_data.iters + new_data.iters;
      decay = Stats.mean_two (old_data.iters, old_data.decay) (new_data.iters, new_data.decay);
      margin = Stats.stderr_two (old_data.iters, old_data.decay, old_data.margin) (new_data.iters, new_data.decay, new_data.margin);
      seed_wins = Lists.sum_two_lists old_data.seed_wins new_data.seed_wins;
      is_fair = new_data.is_fair;
      max_games = new_data.max_games
    }
  else new_data
  in

  old_json
  |> Json.set_key name (data_to_json combined_data)
  |> Json.write ~luck ~number_of_teams
;;

let analyze_schemes ~(luck : float) ~(iters : int) (schemes : Scheme.t list) : unit =
  let n = ref 0 in
  List.iter (fun scheme ->
    Math.inc_ref n;
    print_endline (Int.to_string !n);
    analyze_scheme ~luck ~iters scheme
  ) schemes
;;

let pareto_report ~(luck : float) ~(number_of_teams : int) ~(max_games : int) : unit =
  Json.read ~luck ~number_of_teams
  |> Json.to_object
  |> List.map (fun (name, json) -> name, json_to_data json)
  |> List.filter (fun (_, data) -> data.max_games <= max_games)
  |> List.map (fun (name, data) -> (name, data.decay, if data.is_fair then 0. else Stats.normed_stdev (List.map Int.to_float data.seed_wins)))
  |> Lists.pareto
  |> (fun lst -> if List.length lst = 0 then print_endline "No such formats have been analyzed."; lst)
  |> List.iter (fun (name, decay, imbalance) -> 
      print_endline @@ "(" ^ Math.to_pct ~digits:2 decay ^ ", " ^ Math.to_pct ~digits:2 imbalance ^ "): " ^ name
    )