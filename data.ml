type t = {
  scheme : Scheme.t;
  iters : int;
  luck : float;
  decay : float;
  margin : float;
  seed_wins : int list;
} [@@deriving yojson];;

let calculate_imbalance (data : t) (fair_to_zero : bool) : float =
  let raw = Stats.normed_stdev (List.map Int.to_float data.seed_wins) in
  if fair_to_zero then
    if Scheme.is_fair data.scheme then 0.
    else max 0.0001 (raw -. (Stats.binom_error_formula ~iters:data.iters ~cats:(Scheme.number_of_teams data.scheme)))
  else
    max 0.0001 raw
;;

let record (data : t) =
  print_endline @@ "Hyperparameters: iters = " ^ string_of_int data.iters ^ ", luck = " ^ string_of_float data.luck;
  print_endline @@ "Format: " ^ Scheme.name data.scheme ^ ".";
  print_endline @@ "Decay: " ^ Math.to_pct ~digits:2 data.decay ^ " (" ^ Math.to_pct ~digits:2 data.margin ^ ")" ;
  print_endline @@ "Imbalance: " ^ Math.to_pct ~digits:2 (calculate_imbalance data false) ^ " (" ^ Math.to_pct ~digits:2 (Stats.binom_error_formula ~iters:data.iters ~cats:(Scheme.number_of_teams data.scheme)) ^ ")"

  
let get_file_name ~(number_of_teams : int) ~(luck : float) : string =
  "teams_" ^ Int.to_string number_of_teams ^ "_luck_" ^ Int.to_string (Float.to_int (luck *. 100.))
;;

let write ~(luck : float) ~(number_of_teams : int) (data : t list) : unit =
  print_endline "wti";
  data
  |> List.map yojson_of_t
  |> (fun lst -> `List lst)
  |> Json.write ["analysis"] (get_file_name ~luck ~number_of_teams)
;;

  