open Util;;

let run_one_sim (specs : Data.s) (scheme : Scheme.t) : float =
  let teams = List.init specs.number_of_teams (fun _ -> Team.make ()) in
  let results = Scheme.run scheme teams in

  let best =
    teams
    |> List.map (fun t -> t.Team.skill)
    |> List.sort (Fun.flip compare)
    |> Lists.top_of_list specs.number_advance
    |> Tuple.left
    |> Lists.fold (+.)
  in

  let real =
    results
    |> List.map (fun t -> t.Team.skill)
    |> Lists.top_of_list specs.number_advance
    |> Tuple.left
    |> Lists.fold (+.)
  in

  best -. real
;;

let simulate_scheme (specs : Data.s) (iters : int) (scheme : Scheme.t) : Stats.t =
  Team.set_luck specs.luck;
  Stats.of_list (List.init iters (fun _ -> run_one_sim specs scheme))
;;

let simulate_schemes (specs : Data.s) (iters : int) (schemes : Scheme.t list) : unit =
  schemes
  |> List.map (fun scheme -> scheme, simulate_scheme specs iters scheme)
  |> Data.write specs
;;

let simulate_smart (specs : Data.s) (iters : int) (data : Data.t list) : Data.t list =
  let best =
    data
    |> List.map (fun (_, stats) -> Stats.mean stats)
    |> Lists.fold min
  in
  let shares =
    data
    |> List.map (Tuple.map_right (fun stats -> (Stats.mean stats -. best) /. Stats.stderr stats))
    |> List.map (Tuple.map_right (fun i -> 1. /. (2. ** i)))
  in
  let total =
    shares
    |> List.map Tuple.right
    |> Lists.fold (+.)
  in
  shares
  |> List.map (Tuple.map_right (fun i -> int_of_float (i /. total *. (float_of_int iters))))
  |> List.map (fun (scheme, i) -> (scheme, simulate_scheme specs i scheme))
;;

let simulate_smart_looped (specs : Data.s) (iters : int) =
  Debug.loop (fun () ->
    Data.read specs
    |> simulate_smart specs iters
    |> Data.write specs;
    print_endline "cycle complete"
  )
;;

(*

  let rec sim_smart  ~(luck : float) ~(number_of_teams : int) ~(max_games : int) ~(iters : int) ~(batch_size : int) : unit =
  let pareto_list = Report.get_pareto_list ~luck ~number_of_teams ~max_games in

  let rec p_helper (lst : (Scheme.t * float * float) list) (data : Data.s) : Scheme.t * float = 
    let imb = Data.calculate_imbalance data true in
    match lst with
    | [] -> System.error ()
    | (_, c, d) :: _ when d <= imb -> data.scheme, 1. /. Float.pow 2. ((data.decay -. c) /. data.margin)
    | _ :: tl -> p_helper tl data
  in

  let data = Data.read_data_list ~luck ~number_of_teams ~max_games true in
  let total = List.fold_left (fun x (d : Data.s) -> x + d.iters) 0 data in
  
  data
  |> List.map (p_helper pareto_list) 
  |> Stats.sample batch_size
  |> sim_schemes ~luck ~iters;

  print_endline @@ "Total sims = " ^ Int.to_string (total + iters * batch_size);

  sim_smart ~luck ~number_of_teams ~max_games ~iters ~batch_size
;;



let simulate_schemes (specs : Data.s) (schemes : (int * Scheme.t) list) : Data.t list =
  List.map (fun (iters, scheme) -> scheme, simulate_scheme specs iters scheme) schemes
;;

let 



let simulate_scheme (scheme : Scheme.t) (specs : Data.s) : Data.s =
  Team.set_luck specs.luck;



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
 (* print_endline @@ string_of_int iters_left; *)
  run_sims scheme (iters_left - 1) (decay :: decays) seed_wins
;;

let sim_scheme ~(luck : float) ~(iters : int) (scheme : Scheme.t) : Data.s =
  Team.set_luck luck;
  let seed_wins = Array.make (Scheme.number_of_teams scheme) 0 in
  let sims = run_sims scheme iters [] seed_wins in
  {
    iters;
    luck;
    decay = Stats.mean sims;
    margin = Stats.stderr sims;
    seed_wins = Array.to_list seed_wins;
    scheme = scheme;
  }
;;*)