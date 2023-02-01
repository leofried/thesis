let rec get_best_team_skill (teams : Team.t list) : float =
  match teams with
  | [] -> assert false
  | [t] -> Team.get_skill t
  | hd :: tl -> Float.max (Team.get_skill hd) (get_best_team_skill tl)
;;

let rec run_sims (scheme : Scheme.t) (iters_left : int) (decays: float list) (seed_wins : int array) : float list =
  if iters_left = 0 then decays else
  let teams = List.init (Scheme.number_of_teams scheme) (fun _ -> Team.make ()) in
  let winner = List.hd @@ (Scheme.run scheme) teams in
  let decay = get_best_team_skill teams -. Team.get_skill winner in
  Math.inc_array seed_wins (Lists.find winner teams);
  print_endline @@ string_of_int iters_left;
  run_sims scheme (iters_left - 1) (decay :: decays) seed_wins
;;

let sim_scheme ~(luck : float) ~(iters : int) (scheme : Scheme.t) : Data.t =
  Team.set_luck luck;
  let seed_wins = Array.make (Scheme.number_of_teams scheme) 0 in
  let sims = run_sims scheme iters [] seed_wins in
  {
    iters;
    decay = Stats.mean sims;
    margin = Stats.stderr sims;
    seed_wins = Array.to_list seed_wins;
    scheme = scheme;
  }
;;