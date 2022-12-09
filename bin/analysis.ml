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

let get_data ~(iters : int) (scheme : Scheme.t) : Json.t =
  let seed_wins = Array.make (scheme.number_of_teams) 0 in
  let sims = run_sims scheme iters [] seed_wins in

  `Assoc [
    ("iters", `Int iters);
    ("decay", `Float (Stats.mean sims));
    ("margin", `Float (Stats.stderr sims));
    ("seed_wins", `List (List.map (fun x -> `Int x) (Array.to_list seed_wins)));
    ("is_fair", `Bool (scheme.is_fair (scheme.number_of_teams)));
    ("max_games", `Int (scheme.max_games))
  ]
;;

let get_data_from_json (json : Json.t) : int * float * float * int list =
  Json.rip_int "iters" json,
  Json.rip_float "decay" json,
  Json.rip_float "margin" json,
  Json.rip_int_list "seed_wins" json
;;

let analyze_scheme ?(luck : float = 1.) ~(iters : int) (scheme : Scheme.t) : unit =
  if iters < 2 then System.error ();
  Team.set_luck luck;

  let number_of_teams = scheme.number_of_teams in
  let name = scheme.name in
  let data = get_data ~iters scheme in

  let old_json = Json.read ~luck ~number_of_teams in
  let new_json =
    if Json.has_key name old_json then begin
      let jiters, jdecay, jmargin, jseed_wins = get_data_from_json (Json.member name old_json) in
      let diters, ddecay, dmargin, dseed_wins = get_data_from_json data in

      Json.overwrite_key name (`Assoc [
        ("iters", `Int (jiters + diters));
        ("decay", `Float (Stats.mean_two (jiters, jdecay) (diters, ddecay)));
        ("margin", `Float (Stats.stderr_two (jiters, jdecay, jmargin) (diters, ddecay, dmargin)));
        ("seed_wins", `List (List.map (fun x -> `Int x) (Lists.sum_two_lists jseed_wins dseed_wins)));
        ("is_fair", Json.member "is_fair" data);
        ("max_games", Json.member "max_games" data)
      ]) old_json
  end else
    Json.combine old_json (`Assoc [name, data])
  in
  Json.write ~luck ~number_of_teams new_json
;;

let analyze_schemes ?(luck : float = 1.) ~(iters : int) (schemes : Scheme.t list) : unit =
  let n = ref 0 in
  List.iter (fun scheme ->
    Math.inc_ref n;
    print_endline (Int.to_string !n);
    analyze_scheme ~luck ~iters scheme
  ) schemes
;;