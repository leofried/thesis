open Util;;

let rec get_best_team_skill (teams : Team.t list) : float =
  match teams with
  | [] -> System.error ()
  | [t] -> Team.get_skill t
  | hd :: tl -> Float.max (Team.get_skill hd) (get_best_team_skill tl)
;;

let rec run_sims (scheme : Scheme.t) (iters_left : int) (decays: float list) (seed_wins : int array) : float list =
  if iters_left = 0 then decays else
  let teams = List.init (Scheme.number_of_teams scheme) (fun _ -> Team.make ()) in
  let winner = List.hd @@ Scheme.run scheme teams in
  let decay = get_best_team_skill teams -. Team.get_skill winner in
  Math.inc_array seed_wins (Lists.find winner teams);
  run_sims scheme (iters_left - 1) (decay :: decays) seed_wins
;;

let prog = ref 0;; 

let analyze_scheme  ~(iters : int) ~(number_of_teams : int) (scheme : Scheme.t) : string * Json.t =
  Math.inc_ref prog;
  print_endline (Int.to_string !prog);
  if Scheme.number_of_teams scheme <> number_of_teams then System.error();

  let seed_wins = Array.make (Scheme.number_of_teams scheme) 0 in
  let sims = run_sims scheme iters [] seed_wins in

  Scheme.to_string scheme, `Assoc [
    ("iters", `Int iters);
    ("decay", `Float (Stats.mean sims));
    ("margin", `Float (Stats.stderr sims));
    ("seed_wins", `List (List.map (fun x -> `Int x) (Array.to_list seed_wins)));
    ("is_fair", `Bool (Scheme.is_fair scheme number_of_teams));
    ("max_games", `Int (Scheme.max_games scheme))
  ]
;;

let get_data_from_json (json : Json.t) : int * float * float * int list =
  Json.write ~luck:0. ~number_of_teams:0 json;
  Json.rip_int "iters" json,
  Json.rip_float "decay" json,
  Json.rip_float "margin" json,
  Json.rip_int_list "seed_wins" json
;;

let add_data_to_json (json : Json.t) (name, data : string * Json.t) : Json.t =
  if Json.has_key name json then begin
      let jiters, jdecay, jmargin, jseed_wins = get_data_from_json (Json.member name json) in
      let diters, ddecay, dmargin, dseed_wins = get_data_from_json data in

      Json.overwrite_key name (`Assoc [
        ("iters", `Int (jiters + diters));
        ("decay", `Float (Stats.mean_two (jiters, jdecay) (diters, ddecay)));
        ("margin", `Float (Stats.stderr_two (jiters, jdecay, jmargin) (diters, ddecay, dmargin)));
        ("seed_wins", `List (List.map (fun x -> `Int x) (Lists.sum_two_lists jseed_wins dseed_wins)));
        ("is_fair", Json.member "is_fair" data);
        ("max_games", Json.member "max_games" data)
      ]) json
  end else
    Json.combine json (`Assoc [name, data])
;;

let analyze_schemes ?(luck : float = 1.) ~(iters : int) (schemes : Scheme.t list) : unit =
  Team.set_luck luck;
  print_endline @@ "Total: " ^ Int.to_string @@ List.length @@ schemes;

  let number_of_teams = Scheme.number_of_teams (List.hd schemes) in

  schemes
  |> List.map (analyze_scheme ~iters ~number_of_teams)
  |> List.fold_left add_data_to_json (Json.read ~luck ~number_of_teams)
  |> Json.write ~luck ~number_of_teams
;;