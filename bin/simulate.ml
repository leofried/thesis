open Util;;
open Data;;

(*dont lose data on process kill: backup?
   plus only read once*)

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

(*clean up this progression*)

let get_data ~(iters : int) (scheme : Scheme.t) : Data.t =
  let seed_wins = Array.make (scheme.number_of_teams) 0 in
  let sims = run_sims scheme iters [] seed_wins in
  {
    iters;
    decay = Stats.mean sims;
    margin = Stats.stderr sims;
    seed_wins = Array.to_list seed_wins;
    scheme = scheme;
  }
;;

let combine_data (new_data : Data.t) (old_data : Data.t) : Data.t =
  {
    iters = old_data.iters + new_data.iters;
    decay = Stats.mean_two (old_data.iters, old_data.decay) (new_data.iters, new_data.decay);
    margin = Stats.stderr_two (old_data.iters, old_data.decay, old_data.margin) (new_data.iters, new_data.decay, new_data.margin);
    seed_wins = Lists.sum_two_lists old_data.seed_wins new_data.seed_wins;
    scheme = new_data.scheme;
  }

let sim_scheme ~(luck : float) ~(iters : int) (scheme : Scheme.t) : unit =
  if iters < 2 then System.error ();
  Team.set_luck luck;

  let number_of_teams = scheme.number_of_teams in
  let new_data = get_data ~iters scheme in

  Data.get_data_list ~luck ~number_of_teams false
  |> List.fold_left_map (fun found old_data -> if old_data.scheme.json = scheme.json then true, combine_data new_data old_data else found, old_data) false
  |> (fun (found, lst) -> if found then lst else new_data :: lst)
  |> List.map Data.data_to_json
  |> (fun lst -> `List lst)
  |> Json.write ~luck ~number_of_teams
;;


let sim_schemes ~(luck : float) ~(iters : int) (schemes : Scheme.t list) : unit =
  let n = ref 0 in
  List.iter (fun scheme ->
    Math.inc_ref n;
    print_endline @@ (Int.to_string !n) ^ "/" ^ Int.to_string (List.length schemes);
    sim_scheme ~luck ~iters scheme
  ) schemes
;;



let rec sim_smart  ~(luck : float) ~(number_of_teams : int) ~(max_games : int) ~(iters : int) : unit =
  let pareto_list = Report.get_pareto_list ~luck ~number_of_teams ~max_games in

  let rec p_helper (lst : (Scheme.t * float * float) list) (data : Data.t) : Scheme.t * float = 
    let imb = Data.calculate_imbalance data true in
    match lst with
    | [] -> System.error ()
    | (_, c, d) :: _ when d <= imb -> data.scheme, 1. /. Float.pow 2. ((data.decay -. c) /. data.margin)
    | _ :: tl -> p_helper tl data
  in

 (* begin if Random.bool () then
     (List.map (fun (name, _, _) -> name, 1.) pareto_list)
  else*)
  Data.get_data_list ~luck ~number_of_teams ~max_games true
  |> List.map (p_helper pareto_list) 
  |> Stats.sample
  |> (fun x -> print_endline x.name; x)
  |> sim_scheme ~luck ~iters;

  sim_smart ~luck ~number_of_teams ~max_games ~iters
;;