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
  Math.inc_ref prog;
  print_endline (Int.to_string !prog);
  
  if Scheme.number_of_teams scheme <> number_of_teams then System.error();
  let name = Scheme.to_string scheme in
  let seed_wins = Array.make (Scheme.number_of_teams scheme) 0 in
  let sims = run_sims scheme iters [] seed_wins in
  let decay = Stats.mean sims in
  let margin = Stats.stderr sims in
  let imbalance = Stats.normed_stdev (List.map Int.to_float (Array.to_list seed_wins)) in
  let is_fair = Scheme.is_fair scheme number_of_teams in
  name, (decay, margin), (imbalance, is_fair)
;;

let do_scheme_analysis ~(iters : int) ~(number_of_teams : int) (schemes : Scheme.t list) () =
  print_endline @@ "Total: " ^ Int.to_string @@ List.length @@ schemes;

  let data = List.map (analyze_scheme ~iters ~number_of_teams) schemes in
  let sorted = List.sort (fun (_, (a, _), _) (_, (b, _), _) -> Float.compare a b) data in
  let pareto = Lists.pareto @@ List.map (fun (name, (decay, _), (imbalance, is_fair)) -> (name, decay, if is_fair then 0. else imbalance)) data in
  let raw = List.map (fun (name, (decay, _), (imbalance, _)) -> (name, decay, imbalance)) sorted in

  let _, (_, margin), _ = List.hd sorted in
  let error =  Stats.binom_error ~iters ~cats:number_of_teams () in

  (margin, error), pareto, raw
;;

let print_scheme_analysis print ((margin, error), pareto, raw) =
  let print_line (name, decay, imbalance) =
    print @@
    "(" ^
    Math.to_pct ~digits:2 decay ^
    ", " ^
    Math.to_pct ~digits:2 imbalance ^
    "): " ^
    name      
  in
  
  print @@ "Decay Error: " ^ Math.to_pct ~digits:2 margin;
  print @@ "Imbalance Error: " ^ Math.to_pct ~digits:2 error;
  print @@ "";
  print @@ "Paretos";
  List.iter print_line pareto;
  print @@ "";
  print @@ "Raw";
  List.iter print_line raw;
  print @@ ""
;;

let analyze_schemes ?(luck : float = 1.) ~(iters : int) (schemes : Scheme.t list) : unit =
  Team.set_luck luck;

  let number_of_teams = Scheme.number_of_teams (List.hd schemes) in
  let max_games = List.fold_left max 0 (List.map Scheme.max_games schemes) in

  let data, time = System.time @@ (do_scheme_analysis ~iters ~number_of_teams schemes) in

  let channel = open_out (
    "analysis/" ^
    Float.to_string (Unix.time ()) ^
    Int.to_string number_of_teams ^ "teams_" ^
    Int.to_string max_games ^ "games_" ^
    Float.to_string luck ^ "luck_" ^
    Int.to_string iters ^ "iters_" ^
    ".txt") in
  let print = (fun x -> output_string channel (x ^ "\n")) in

  print @@ "Teams: " ^ Int.to_string number_of_teams;
  print @@ "Games: " ^ Int.to_string max_games;
  print @@ "Luck: " ^ Float.to_string luck;
  print @@ "Iters: " ^ Int.to_string iters;
  print @@ "Time: " ^ Float.to_string time;
  print @@ "";

  print_scheme_analysis print data;
  flush channel
;;
(*
let analyze_schemes ?(luck : float = 1.) ~(iters : int) (schemes : Scheme.t list) : unit =
  System.time @@ (fun () ->
    Team.set_luck luck;

    let number_of_teams = Scheme.number_of_teams (List.hd schemes) in

    print_endline @@ "Total: " ^ Int.to_string @@ List.length @@ schemes;
    let data = List.map (analyze_scheme ~iters ~number_of_teams) schemes in
    let sorted = List.sort (fun (_, a, b, _) (_, c, d, _) -> Float.compare (a +. b) (c +. d)) data in

    let _, _, _, margin = List.hd sorted in
    let error =  Stats.binom_error ~iters ~cats:number_of_teams () in

    let channel = open_out ("analysis/analysis" ^ Float.to_string (Unix.time ()) ^ "txt") in
    output_string channel @@ "Teams: " ^ Int.to_string number_of_teams ^ "\n";
    output_string channel @@ "Luck: " ^ Float.to_string luck ^ "\n";
    output_string channel @@ "Iters: " ^ Int.to_string iters ^ "\n";
    output_string channel @@ "Decay Error: " ^ Math.to_pct ~digits:2 margin ^ "\n";
    output_string channel @@ "Imbalance Error: " ^ Math.to_pct ~digits:2 error ^ "\n\n";

    let print (name, decay, imbalance) = 
      output_string channel @@
      "(" ^
      Math.to_pct ~digits:2 decay ^
      ", " ^
      Math.to_pct ~digits:2 imbalance ^
      ") = " ^
      Math.to_pct ~digits:2 (decay +. imbalance) ^
      ": " ^
      name ^
      "\n"
    in

    List.iter print @@ Lists.pareto (List.map (fun (name, decay, imbalance, _) -> (name, decay, Math.pos_sub imbalance error)) data);
    output_string channel "\n";
    List.iter print @@ (List.map (fun (name, decay, imbalance, _) -> (name, decay, imbalance)) sorted);

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