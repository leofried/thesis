open Util;;

module M (Scheme : S.SCHEME) = struct

  type t = Scheme.t * Stats.t [@@deriving yojson];;

  let max_games n scheme =
    n
    |> Team.make_n 0.
    |> Scheme.run scheme
    |> List.map (fun t -> t.Team.games)
    |> Lists.fold max
  ;;

  let get_file_name (specs : Specs.t) =
    Scheme.kind ^ "_" ^
    string_of_int specs.number_of_teams ^ "_teams_" ^
    string_of_int specs.number_advance ^ "_advance_" ^
    string_of_float specs.luck ^ "_luck_" ^
    string_of_float specs.fidel ^ "_fidel"
  ;;

  let read (specs : Specs.t) =
    Json.read ["analysis"] (get_file_name specs)
    |> Option.value ~default:(`List [])
    |> Json.to_list
    |> List.map t_of_yojson
    |> List.filter (fun (scheme, _) -> max_games specs.number_of_teams scheme <= specs.max_games)
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
          Scheme.to_string scheme
        )
        (List.sort
          (fun ((_, s1) : t) ((_, s2) : t) -> Float.compare (Stats.mean s2) (Stats.mean s1))
          (read specs)
        )
  ;;
end
