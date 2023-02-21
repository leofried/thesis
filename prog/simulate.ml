open Util;;
open Struct;;

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
  run_sims scheme (iters_left - 1) (decay :: decays) seed_wins
;;

let get_data ~(iters : int) (scheme : Scheme.t) : Data.t =
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

let sim_scheme ~(iters : int) (lst : Data.t list) (scheme : Scheme.t) : Data.t list =
  let new_data = get_data ~iters scheme in
  let found, new_lst = List.fold_left_map
    (fun found (old_data : Data.t) ->
      if Scheme.equals old_data.scheme scheme then
        true, Data.combine_datum new_data old_data
      else 
        found, old_data
    )
    false lst
  in
  
  if found then new_lst else new_data :: new_lst
;;


let sim_schemes ~(luck : float) ~(iters_pow : int) (schemes : Scheme.t list) : unit =
  if iters_pow < 1 then assert false;
  Team.set_luck luck;
  
  let number_of_teams = (Scheme.number_of_teams (List.hd schemes)) in
  let count = ref 0 in
  let total = (List.length schemes) in

  List.fold_left 
    (fun data (scheme : Scheme.t) ->
      if (Scheme.number_of_teams scheme) <> number_of_teams then assert false;
      Math.inc_ref count;
      print_endline @@ (Int.to_string !count) ^ "/" ^ Int.to_string total ^ ": " ^ Scheme.name scheme;
      sim_scheme ~iters:(Math.pow 10 iters_pow) data scheme
    )
    (Data.read ~luck ~number_of_teams false)
    schemes
  |> Data.write ~luck ~number_of_teams  
;;

let rec sim_smart  ~(luck : float) ~(number_of_teams : int) ~(max_games : int) ~(iters_pow : int) ~(batch_size : int) : unit =
  let pareto_list = Report.get_pareto_list ~luck ~number_of_teams ~max_games in

  let rec p_helper (lst : (Scheme.t * float * float) list) (data : Data.t) : Scheme.t * float = 
    let imb = Report.calculate_imbalance data true in
    match lst with
    | [] -> assert false
    | (_, c, d) :: _ when d <= imb -> data.scheme, 1. /. Float.pow 2. ((data.decay -. c) /. data.margin)
    | _ :: tl -> p_helper tl data
  in

  let data = Data.read ~luck ~number_of_teams ~max_games true in
  let total = List.fold_left (fun x (d : Data.t) -> x + d.iters) 0 data in
  
  data
  |> List.map (p_helper pareto_list) 
  |> Stats.sample batch_size
  |> sim_schemes ~luck ~iters_pow;

  print_endline @@ "Total sims = " ^ Int.to_string (total + (Math.pow 10 iters_pow) * batch_size);

  sim_smart ~luck ~number_of_teams ~max_games ~iters_pow ~batch_size
;;