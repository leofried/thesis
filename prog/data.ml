open Util;;
open Struct;;

type t = {
  scheme : Specs.Scheme.t;
  iters : int;
  decay : float;
  margin : float;
  seed_wins : int list;
} [@@deriving yojson];;

let combine_datum (new_data : t) (old_data : t) : t =
  {
    scheme = new_data.scheme;
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
          if Scheme.equals new_data.scheme old_data.scheme then
            true, combine_datum new_data old_data
          else 
            found, old_data
        )
        false old_lst
      in if found then new_lst else new_data :: new_lst
  ) []
;;

let path = ["analysis"; "data"];;

let get_file_name ~(number_of_teams : int) ~(luck : float) : string =
  "teams_" ^ Int.to_string number_of_teams ^ "_luck_" ^ Int.to_string (Float.to_int (luck *. 100.))
;;

let read ~(luck : float) ~(number_of_teams : int) ?(max_games : int = Int.max_int) (throw : bool) : t list =
  let lst = get_file_name ~luck ~number_of_teams
    |> Json.read path
    |> Option.value ~default:(`List [])
    |> Json.rip_list t_of_yojson
    |> List.filter (fun data -> Scheme.max_games data.scheme <= max_games)
  in
  if throw && List.length lst = 0 then
    raise (Specs.NoSuchFormatsHaveBeen "Analyzed")
  else
    lst
;;

let write ~(luck : float) ~(number_of_teams : int) (data : t list) : unit =
  read ~luck ~number_of_teams false
  |> List.append data
  |> combine_data
  |> Json.place_list yojson_of_t
  |> Json.write path (get_file_name ~luck ~number_of_teams) false
;;