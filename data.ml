type t = {
  scheme : Scheme.t;
  luck : float;
  iters : int;
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

let combine_datum (new_data : t) (old_data : t) : t =
  {
    scheme = new_data.scheme;
    luck = new_data.luck;
    iters = old_data.iters + new_data.iters;
    decay = Stats.mean_two (old_data.iters, old_data.decay) (new_data.iters, new_data.decay);
    margin = Stats.stderr_two (old_data.iters, old_data.decay, old_data.margin) (new_data.iters, new_data.decay, new_data.margin);
    seed_wins = Lists.sum_two_lists old_data.seed_wins new_data.seed_wins;
  }
;;

let combine_data =
  List.fold_left (
    fun old_lst new_data ->
      let found, new_lst = List.fold_left_map
        (fun found old_data ->
          if new_data.luck = old_data.luck && (Scheme.yojson_of_t new_data.scheme) = (Scheme.yojson_of_t old_data.scheme) then
            true, combine_datum new_data old_data
          else 
            found, old_data
        )
        false old_lst
      in if found then new_lst else new_data :: new_lst
  ) []
;;

let read () : t list =
  Json.read ["analysis"] "data"
  |> Option.value ~default:(`List [])
  |> Json.to_list
  |> List.map t_of_yojson
;;

let write (data : t list) : unit =
  print_endline "hi";
  read ()
  |> (@) data
  |> combine_data
  |> List.map yojson_of_t
  |> (fun lst -> `List lst)
  |> (fun x -> print_endline (Yojson.Safe.to_string x); x)
  |> Json.write ["analysis"] "data" true
;;

(*
let record (data : t) =
  print_endline @@ "Hyperparameters: iters = " ^ string_of_int data.iters ^ ", luck = " ^ string_of_float data.luck;
  print_endline @@ "Format: " ^ Scheme.name data.scheme ^ ".";
  print_endline @@ "Decay: " ^ Math.to_pct ~digits:2 data.decay ^ " (" ^ Math.to_pct ~digits:2 data.margin ^ ")" ;
  print_endline @@ "Imbalance: " ^ Math.to_pct ~digits:2 (calculate_imbalance data false) ^ " (" ^ Math.to_pct ~digits:2 (Stats.binom_error_formula ~iters:data.iters ~cats:(Scheme.number_of_teams data.scheme)) ^ ")"
;;

let write ~(luck : float) ~(number_of_teams : int) (data : t list) : unit =
  print_endline "wti";
  data
  |> List.map yojson_of_t
  |> (fun lst -> `List lst)
  |> Json.write ["analysis"] (get_file_name ~luck ~number_of_teams)
;;

  *)