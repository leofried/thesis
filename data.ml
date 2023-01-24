type t = {
  scheme : Scheme.t;
  iters : int;
  decay : float;
  margin : float;
  seed_wins : int list;
};;

let calculate_imbalance (data : t) (fair_to_zero : bool) : float =
  let raw = Stats.normed_stdev (List.map Int.to_float data.seed_wins) in
  if fair_to_zero then
    if data.scheme.is_fair then 0.
    else max 0.0001 (raw -. (Stats.binom_error_formula ~iters:data.iters ~cats:data.scheme.number_of_teams))
  else
    max 0.0001 raw
;;