open Util;;
open Struct;;

type t = {
  scheme : Scheme.t;
  iters : int;
  decay : float;
  margin : float;
  seed_wins : int list;
};;

let json_to_data (json : Json.t) : t =
  {
    scheme = Specs.json_to_scheme (Json.member "scheme" json);
    iters = Json.rip Json.to_int "iters" json;
    decay = Json.rip Json.to_float "decay" json;
    margin = Json.rip Json.to_float "margin" json;
    seed_wins = Json.rip_list Json.to_int "seed_wins" json;
  }
;;

let data_to_json (data : t) : Json.t =
  `Assoc [
    ("scheme", data.scheme.json);
    ("iters", `Int data.iters);
    ("decay", `Float data.decay);
    ("margin", `Float data.margin);
    ("seed_wins", Json.place_int_list data.seed_wins);
  ]
;;

let path = ["analysis"; "data"];;

let get_file_name ~(number_of_teams : int) ~(luck : float) : string =
  "teams_" ^ Int.to_string number_of_teams ^ "_luck_" ^ Int.to_string (Float.to_int (luck *. 100.))
;;

let read ~(luck : float) ~(number_of_teams : int) ?(max_games : int = Int.max_int) (throw : bool) : t list =
  let lst = get_file_name ~luck ~number_of_teams
    |> Json.read path
    |> Option.value ~default:(`List [])
    |> Json.to_list
    |> List.map json_to_data
    |> List.filter (fun data -> data.scheme.max_games <= max_games)
  in
  if throw && List.length lst = 0 then
    raise (Specs.NoSuchFormatsHaveBeen "Analyzed")
  else
    lst
;;

let write ~(luck : float) ~(number_of_teams : int) (data : t list) : unit =
  data
  |> List.map data_to_json
  |> (fun lst -> `List lst)
  |> Json.write path (get_file_name ~luck ~number_of_teams)
;;