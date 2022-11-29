open Util;;

let rec get_best_team_skill (teams : Team.t list) : float =
  match teams with
  | [] -> Throw.error ()
  | [t] -> Team.get_skill t
  | hd :: tl -> Float.max (Team.get_skill hd) (get_best_team_skill tl)

let rec run_sims (scheme : Scheme.t) (iters_left : int) (so_far : float list) : float list =
  if iters_left = 0 then so_far else
  let teams = List.init (Scheme.number_of_teams scheme) (fun _ -> Team.make ()) in
  let winner = List.hd @@ Scheme.run scheme teams in
  let decay = get_best_team_skill teams -. Team.get_skill winner in
  run_sims scheme (iters_left - 1) (decay :: so_far)

let analyze_scheme ?(digits : int = 2) ?(luck : float = 1.) ~(iters : int) (scheme : Scheme.t) : unit =
  Team.set_luck luck;
  let mean, _, stder = Lists.stats (run_sims scheme iters [])  in
  print_endline @@
    "A " ^
    Scheme.to_string scheme ^
    " with luck = " ^
    Float.to_string luck ^
    " has a decay of " ^
    Math.to_pct ~digits mean ^
    "% +/- " ^
    Math.to_pct ~digits stder ^
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
      Math.inc count
  done;
  print_endline @@
    "With luck = " ^
    Float.to_string luck ^
    " the better teams wins " ^ 
    (Int.to_string @@ Float.to_int @@ Float.round (100. *. Math.divide !count iters)) ^ 
    "% of the time."
  ;;
