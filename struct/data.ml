open Util;;

type s = { 
  number_of_teams : int;
  number_advance : int;
  luck : float;
  max_games : int;
};;

let default_specs number_of_teams = {
  number_of_teams;
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
  |> List.filter (fun (scheme, _) -> Scheme.max_games specs.number_of_teams scheme <= specs.max_games)
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