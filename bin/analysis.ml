open Util;;

let rec get_best_team_skill (teams : Team.t list) : float =
  match teams with
  | [] -> System.error ()
  | [t] -> Team.get_skill t
  | hd :: tl -> Float.max (Team.get_skill hd) (get_best_team_skill tl)

let rec run_sims (scheme : Scheme.t) (iters_left : int) (decays: float list) (seed_wins : int array) : float list =
  if iters_left = 0 then decays else
  let teams = List.init (Scheme.number_of_teams scheme) (fun _ -> Team.make ()) in
  let winner = List.hd @@ Scheme.run scheme teams in
  let decay = get_best_team_skill teams -. Team.get_skill winner in
  Math.inc_array seed_wins (Lists.find winner teams);
  run_sims scheme (iters_left - 1) (decay :: decays) seed_wins

let prog = ref 0;;

let analyze_scheme  ~(iters : int) ~(number_of_teams : int) (scheme : Scheme.t) : string * (float * float) * (float * bool) =  
  if Scheme.number_of_teams scheme <> number_of_teams then System.error() else
  Math.inc_ref prog;
  print_endline (Int.to_string !prog);
  
  let name = Scheme.to_string scheme in
  let seed_wins = Array.make (Scheme.number_of_teams scheme) 0 in
  let sims = run_sims scheme iters [] seed_wins in
  let decay = Stats.mean sims in
  let imbalance = Stats.normed_stdev (List.map Int.to_float (Array.to_list seed_wins)) in
  let margin = Stats.stderr sims in
  name, (decay, margin), (imbalance, Scheme.is_fair scheme number_of_teams)
;;

let analyze_schemes_iters ~(iters_now : int) ~(number_of_teams : int) (schemes : Scheme.t list) =
  prog := 0;
  print_endline @@ "We take " ^ Int.to_string (List.length schemes) ^ " to 10^" ^ Int.to_string iters_now;

  let iters = Math.pow 10 iters_now in
  let error = Stats.binom_error ~iters ~cats:number_of_teams () in
  let data = schemes
  |> List.map (fun s -> s, analyze_scheme ~iters ~number_of_teams s)
  |> List.sort (fun (_, (_, (a, _), _)) (_, (_, (b, _), _)) -> Float.compare a b) in
  let basis = List.map (fun (_, (_, (a, _), (b, c))) -> a, if c then 0. else b) data in
  let pareto = List.filter_map (fun (scheme, (_, (decay, margin), (imbalance, is_fair))) -> 
    if Lists.pareto_survive (decay -. 3. *. margin, if is_fair then 0. else imbalance -. error) basis then Some scheme else None
  ) data in
  (error, List.map snd data), pareto
;;

let rec analyze_scheme_iters_all ~(iters_now : int) ~(iters_pow : int) ~(number_of_teams : int) (schemes : Scheme.t list) =
  if iters_now > iters_pow then [] else
  let data, paretos = analyze_schemes_iters ~iters_now ~number_of_teams schemes in
  (iters_now, data) :: analyze_scheme_iters_all ~iters_now:(iters_now + 1) ~iters_pow ~number_of_teams paretos
;;

let rec print_data print data =
  match data with
  | [] -> ()
  | (iters_pow, (error, schemes)) :: lst ->
    print_data print lst;
    print @@ "Ran 10^" ^ Int.to_string iters_pow ^ " iterations (error = " ^ Math.to_pct ~digits:2 error ^ "):";
    List.iter (fun (name, (decay, margin), (imbalance, is_fair)) ->
      print @@
      "(" ^
      Math.to_pct ~digits:2 decay ^
      " [" ^
      Math.to_pct ~digits:2 margin ^
      "], " ^
      Math.to_pct ~digits:2 imbalance ^
      " [" ^
      Bool.to_string is_fair ^
      "]): " ^
      name      
    ) schemes;
    print @@ ""
  ;;

let analyze_schemes ?(luck : float = 1.) ?(iters_start : int = 1) ~(iters_pow : int) (schemes : Scheme.t list) : unit =
  System.time @@ (fun () ->
    Team.set_luck luck;

    let number_of_teams = Scheme.number_of_teams (List.hd schemes) in
    let max_games = List.fold_left max 0 (List.map Scheme.max_games schemes) in

    let data = analyze_scheme_iters_all ~iters_now:iters_start ~iters_pow ~number_of_teams schemes in

    let channel = open_out (
      "analysis/" ^
      Float.to_string (Unix.time ()) ^
      Int.to_string number_of_teams ^ "teams_" ^
      Int.to_string max_games ^ "games_" ^
      Float.to_string luck ^ "luck_10^" ^
      Int.to_string iters_pow ^ "iters_" ^
      ".txt") in
    let print = (fun x -> output_string channel (x ^ "\n")) in

    print @@ "Teams: " ^ Int.to_string number_of_teams;
    print @@ "Games: " ^ Int.to_string max_games;
    print @@ "Luck: " ^ Float.to_string luck;
    print @@ "Iters: 10^" ^ Int.to_string iters_pow;
    print @@ "";

    print_data print data;
    flush channel
  )
;;

(*
let analyze_schemes ?(luck : float = 1.) ~(iters : int) (schemes : Scheme.t list) : unit =
  System.time @@ (fun () ->
    Team.set_luck luck;

    let number_of_teams = Scheme.number_of_teams (List.hd schemes) in

    print_endline @@ "Total: " ^ Int.to_string @@ List.length @@ schemes;
    let data = List.map (analyze_scheme ~iters ~number_of_teams) schemes in
    let sorted = List.sort (fun (_, (a, _), _) (_, (c, _), _) -> Float.compare a c) data in

    let max_games = List.fold_left max 0 (List.map Scheme.max_games schemes) in 
    let _, (_, margin), _ = List.hd sorted in
    let error =  Stats.binom_error ~iters ~cats:number_of_teams () in

    let channel = open_out (
      "analysis/" ^
      Float.to_string (Unix.time ()) ^
      Int.to_string number_of_teams ^ "teams_" ^
      Int.to_string max_games ^ "games_" ^
      Float.to_string luck ^ "luck_" ^
      Int.to_string iters ^ "iters_" ^
      ".txt")
    in
    
    output_string channel @@ "Teams: " ^ Int.to_string number_of_teams ^ "\n";
    output_string channel @@ "Games: " ^ Int.to_string max_games ^ "\n";
    output_string channel @@ "Luck: " ^ Float.to_string luck ^ "\n";
    output_string channel @@ "Iters: " ^ Int.to_string iters ^ "\n";
    output_string channel @@ "Decay Error: " ^ Math.to_pct ~digits:2 margin ^ "\n";
    output_string channel @@ "Imbalance Error: " ^ Math.to_pct ~digits:2 error ^ "\n\n";

    let print (name, decay, margin, imbalance) = 
      output_string channel @@
      "(" ^
      Math.to_pct ~digits:2 decay ^
      " [" ^
      Math.to_pct ~digits:2 margin ^
      "], " ^
      Math.to_pct ~digits:2 imbalance ^
      "): " ^
      name ^
      "\n"
    in

    (*List.iter print @@ Lists.pareto (List.map (fun (name, (decay, _), (imbalance, fair)) -> name, decay, _, if fair then 0. else imbalance) data); 
    *)
    output_string channel "\n";
    List.iter print @@ (List.map (fun (name, (decay, m), (imbalance, _)) -> name, decay, m, imbalance) sorted);

    flush channel
  )
;;
*)


let calculate_better_team_win_pct ~(luck : float) ~(iters : int): unit =
  Team.set_luck luck;
  let count = ref 0 in
  for _ = 1 to iters do
    let t1 = Team.make () in
    let t2 = Team.make () in
    let w, l = Team.play_game t1 t2 in
    if (Team.get_skill w) > (Team.get_skill l) then
      Math.inc_ref count
  done;
  print_endline @@
    "With luck = " ^
    Float.to_string luck ^
    " the better teams wins " ^ 
    (Int.to_string @@ Float.to_int @@ Float.round (100. *. Math.divide_int_int !count iters)) ^ 
    "% of the time."
;;