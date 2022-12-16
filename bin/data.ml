open Util;;

type t = {
  scheme : Scheme.t;
  iters : int;
  decay : float;
  margin : float;
  seed_wins : int list;
};;

let json_to_scheme (json : Json.t) : Scheme.t =
  let kind = Json.rip_string "kind" json in
  let rec f = function
    | [] -> System.error ()
    | hd :: tl ->
      let module Mod = (val hd : Scheme.kind) in
      if Mod.kind = kind then Mod.make_from_json json else f tl
 in f [(module Round_robin); (module Bracket); (module Pool_play)]
;;

let json_to_data (json : Json.t) : t =
  {
    scheme = json_to_scheme (Json.member "scheme" json);
    iters = Json.rip_int "iters" json;
    decay = Json.rip_float "decay" json;
    margin = Json.rip_float "margin" json;
    seed_wins = Json.rip_list "seed_wins" json;
  }
;;

let data_to_json (data : t) : Json.t =
  `Assoc [
    ("scheme", data.scheme.json);
    ("iters", `Int data.iters);
    ("decay", `Float data.decay);
    ("margin", `Float data.margin);
    ("seed_wins", Json.place_list data.seed_wins);
  ]
;;

let calculate_imbalance (data : t) (fair_to_zero : bool) : float =
  if fair_to_zero && data.scheme.is_fair then 0. else Stats.normed_stdev (List.map Int.to_float data.seed_wins)
;;

let read_data_list ~(luck : float) ~(number_of_teams : int) ?(max_games : int = Int.max_int) (throw : bool) : t list =
  let lst = Json.read_analysis ~luck ~number_of_teams
    |> Json.to_list
    |> List.map json_to_data
    |> List.filter (fun data -> data.scheme.max_games <= max_games)
  in
  if throw && List.length lst = 0 then begin
    print_endline "No such formats have been analyzed.";
    System.error ()
  end else
    lst
;;

let write_data_list ~(luck : float) ~(number_of_teams : int) (data : t list) : unit =
  data
  |> List.map data_to_json
  |> (fun lst -> `List lst)
  |> Json.write_analysis ~luck ~number_of_teams
;;
