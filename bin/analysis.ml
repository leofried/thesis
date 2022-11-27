let rec get_best_team_skill (teams : Team.t list) : float =
  match teams with
  | [] -> Util.error ()
  | [t] -> Team.get_skill t
  | hd :: tl -> Float.max (Team.get_skill hd) (get_best_team_skill tl)

let rec run_sims (scheme : Scheme.t) (iters_left : int) (cum : float) : float =
  if iters_left = 0 then cum else
  let teams = List.init (Scheme.number_of_teams scheme) (fun _ -> Team.make ()) in
  let winner = List.hd @@ Scheme.run scheme teams in
  let decay = get_best_team_skill teams -. Team.get_skill winner in
  run_sims scheme (iters_left - 1) (cum +. decay)

let analyze_scheme ?(luck : float = 1.) ~(iters : int) (scheme : Scheme.t) : unit =
  Team.set_luck luck;
  let score = (run_sims scheme iters 0.) /. (Int.to_float iters) in
  print_endline @@
    "A " ^
    Scheme.to_string scheme ^
    " with luck = " ^
    Float.to_string luck ^
    " has a decay of " ^
    Float.to_string (Float.round (score *. 1000.) /. 10.) ^
    "%."
;;

let calculate_better_team_win_pct ~(luck : float) ~(iters : int): unit =
  Team.set_luck luck;
  let count = ref 0 in
  for _ = 1 to iters do
    let t1 = Team.make () in
    let t2 = Team.make () in
    let w, l = Team.play_game t1 t2 in
    if Float.compare (Team.get_skill w) (Team.get_skill l) > 0 then
      Util.inc count
  done;
  print_endline @@
    "With luck = " ^
    Float.to_string luck ^ 
    " the better teams wins " ^ 
    (Int.to_string @@ Float.to_int @@ Float.round (100. *. Util.divide !count iters)) ^ 
    "% of the time."
  ;;
