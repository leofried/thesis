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

let sim_scheme ~(iters : int) (lst : Data.t list) (scheme : Scheme.t) : Data.t list =
  let new_data = get_data ~iters scheme in
  let found, new_lst = List.fold_left_map
    (fun found (old_data : Data.t) ->
      if old_data.scheme.json = scheme.json then
        true, combine_data new_data old_data
      else 
        found, old_data
    )
    false lst
  in
  
  if found then new_lst else new_data :: new_lst
;;


let sim_schemes ~(luck : float) ~(iters : int) (schemes : Scheme.t list) : unit =
  if iters < 2 then System.error ();
  Team.set_luck luck;
  
  let number_of_teams = ((List.hd schemes).number_of_teams) in
  let count = ref 0 in
  let total = (List.length schemes) in

  List.fold_left 
    (fun data (scheme : Scheme.t) ->
      if scheme.number_of_teams <> number_of_teams then System.error();
      Math.inc_ref count;
      print_endline @@ (Int.to_string !count) ^ "/" ^ Int.to_string total ^ ": " ^ scheme.name;
      sim_scheme ~iters data scheme
    )
    (Data.read_data_list ~luck ~number_of_teams false)
    schemes
  |> Data.write_data_list ~luck ~number_of_teams  
;;

let sim_specs ~(name : string) ~(luck : float) ~(iters : int) : unit =
  Json.read_specs ~name
  |> Json.to_list
  |> List.map Data.json_to_scheme
  |> sim_schemes ~luck ~iters
;;

let rec sim_smart  ~(luck : float) ~(number_of_teams : int) ~(max_games : int) ~(iters : int) ~(batch_size : int) : unit =
  let pareto_list = Report.get_pareto_list ~luck ~number_of_teams ~max_games in

  let rec p_helper (lst : (Scheme.t * float * float) list) (data : Data.t) : Scheme.t * float = 
    let imb = Data.calculate_imbalance data true in
    match lst with
    | [] -> System.error ()
    | (_, c, d) :: _ when d <= imb -> data.scheme, 1. /. Float.pow 2. ((data.decay -. c) /. data.margin)
    | _ :: tl -> p_helper tl data
  in

  let data = Data.read_data_list ~luck ~number_of_teams ~max_games true in
  let total = List.fold_left (fun x (d : Data.t) -> x + d.iters) 0 data in
  
  data
  |> List.map (p_helper pareto_list) 
  |> Stats.sample batch_size
  |> sim_schemes ~luck ~iters;

  print_endline @@ "Total sims = " ^ Int.to_string (total + iters * batch_size);

  sim_smart ~luck ~number_of_teams ~max_games ~iters ~batch_size
;;