open Util;;
open Ops;;

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

let analyze_scheme  ~(iters : int) ~(number_of_teams : int) (scheme : Scheme.t) : string * float * float * float =
  print_endline (Int.to_string !prog);
  Math.inc_ref prog;
  if Scheme.number_of_teams scheme <> number_of_teams then System.error();
  let name = Scheme.to_string scheme in
  let seed_wins = Array.make (Scheme.number_of_teams scheme) 0 in
  let sims = run_sims scheme iters [] seed_wins in
  let decay = Stats.mean sims in
  let margin = Stats.stderr sims in
  let imbalance = Stats.normed_stdev (List.map Int.to_float (Array.to_list seed_wins)) in
  name, decay, imbalance, margin
;;

let analyze_schemes ?(luck : float = 1.) ~(iters : int)  (schemes : Scheme.t list) : unit =
  Team.set_luck luck;
  print_endline @@ Int.to_string @@ List.length @@ schemes;
  let number_of_teams = Scheme.number_of_teams (List.hd schemes) in
  let data = List.map (analyze_scheme ~iters ~number_of_teams) schemes in
  let sorted = List.sort (fun (_, a, b, _) (_, c, d, _) -> Float.compare (a +. b) (c +. d)) data in
  let _, d, i, m = List.hd sorted in
  let error = m +. Stats.binom_error ~iters ~cats:number_of_teams () in
  let cutoff = ref (d +. i +. error, 0) in
  List.iter (fun (name, decay, imbalance, _) -> 
    if snd !cutoff < 1 then begin
      if decay +. imbalance >. fst !cutoff then begin
        print_endline "------------";
        cutoff := decay +. imbalance +. error, snd !cutoff + 1
      end;
      if snd !cutoff < 1 then begin
        print_endline @@
        "(" ^
        Math.to_pct ~digits:2 decay ^
        ", " ^
        Math.to_pct ~digits:2 imbalance ^
        ") = " ^
        Math.to_pct ~digits:2 (decay +. imbalance) ^
        ": " ^
        name
      end
    end
    ) sorted
;;

let calculate_better_team_win_pct ~(luck : float) ~(iters : int): unit =
  Team.set_luck luck;
  let count = ref 0 in
  for _ = 1 to iters do
    let t1 = Team.make () in
    let t2 = Team.make () in
    let w, l = Team.play_game t1 t2 in
    if (Team.get_skill w) >. (Team.get_skill l) then
      Math.inc_ref count
  done;
  print_endline @@
    "With luck = " ^
    Float.to_string luck ^
    " the better teams wins " ^ 
    (Int.to_string @@ Float.to_int @@ Float.round (100. *. Math.divide_int_int !count iters)) ^ 
    "% of the time."
;;

(*
   let analyze_schemes ?(luck : float = 1.) ~(iters : int)  (schemes : Scheme.t list) : unit =
  Team.set_luck luck;
  let data = List.map (analyze_scheme
    ~iters
    ~number_of_teams: Scheme.number_of_teams (List.hd schemes)
    ~error: Stats.binom_error ~iters ~cats:number_of_teams ())
    schemes in
  let sorted = List.sort (fun (_, a, b, _) (_, c, d, _) -> Float.compare (a +. b) (c +. d)) data in
  let _, d, i, m = List.hd sorted in
  let cutoff = ref (d +. i +. m, 0) in
  List.iter (fun (name, decay, imbalance, _) -> 
    if snd !cutoff < 1 then begin
      if decay +. imbalance >. fst !cutoff then begin
        print_endline "------------";
        cutoff := decay +. imbalance +. error, snd !cutoff + 1
      end;
      if snd !cutoff < 1 then begin
        print_endline @@
        "(" ^
        Math.to_pct ~digits:2 decay ^
        ", " ^
        Math.to_pct ~digits:2 imbalance ^
        ") = " ^
        Math.to_pct ~digits:2 (decay +. imbalance) ^
        ": " ^
        name
      end
    end
    ) sorted
;;*)