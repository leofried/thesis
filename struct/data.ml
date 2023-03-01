open Util;;

type s = { 
  number_of_teams : int;
  number_advance : int;
  luck : float;
  max_games : int;
};;

let default_specs = {
  number_of_teams = -1;
  number_advance = 1;
  luck = 1.;
  max_games = Int.max_int;
}

type t = Scheme.t * Stats.t [@@deriving yojson];;

let get_file_name {number_of_teams; number_advance; luck; _} =
  string_of_int number_of_teams ^ "_teams_" ^
  string_of_int number_advance ^ "_advance_" ^
  Math.to_pct luck
;;

let read specs =
  Json.read ["analysis"] (get_file_name specs)
  |> Option.value ~default:(`List [])
  |> Json.to_list
  |> List.map t_of_yojson
  |> List.filter (fun (scheme, _) -> Scheme.max_games scheme specs.number_of_teams <= specs.max_games)
;;

let combine_data =
  List.fold_left (
    fun old_lst (new_scheme, new_data) ->
      let found, new_lst = List.fold_left_map
        (fun found (old_scheme, old_data) ->
          if new_scheme = old_scheme then
            true, (old_scheme, Stats.combine new_data old_data)
          else 
            found, (old_scheme, old_data)
        )
        false old_lst
      in if found then new_lst else (new_scheme, new_data) :: new_lst
  ) []
;;

let write specs (data : t list) : unit =
  specs
  |> read
  |> (@) data
  |> combine_data
  |> Json.place_list yojson_of_t
  |> Json.write ["analysis"] (get_file_name specs) true
;;

let print specs : unit =
    List.iter
      (fun ((scheme, stats) : t) -> 
        print_endline @@
        "" ^ 
        Math.to_pct ~digits:2 (Stats.mean stats) ^
        " [" ^
        Math.to_pct ~digits:2 (Stats.stderr stats) ^
        "], on " ^
        Int.to_string stats.samples ^
        " iters : " ^
        Scheme.name scheme
      )
      (List.sort
        (fun ((_, s1) : t) ((_, s2) : t) -> Float.compare (Stats.mean s2) (Stats.mean s1))
        (read specs)
      )
  ;;

(*
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

let read_filter ~luck ~number_of_teams ~max_games : t list =
  read ()
  |> List.filter (fun data -> data.luck = luck)
  |> List.filter (fun data -> (Scheme.number_of_teams data.scheme = number_of_teams))
  |> List.filter (fun data -> (Scheme.max_games data.scheme <= max_games))
;;

let write (data : t list) : unit =
  read ()
  |> (@) data
  |> combine_data
  |> List.map yojson_of_t
  |> (fun lst -> `List lst)
  |> Json.write ["analysis"] "data" true
;;*)