open Util;;

type t = {
  iters : int;
  decay : float;
  margin : float;
  seed_wins : int list;
  is_fair : bool;
  max_games : int;
}

let json_to_data (json : Json.t) : t =
  {
    iters = Json.rip_int "iters" json;
    decay = Json.rip_float "decay" json;
    margin = Json.rip_float "margin" json;
    seed_wins = Json.rip_int_list "seed_wins" json;
    is_fair = Json.rip_bool "is_fair" json;
    max_games = Json.rip_int "max_games" json
  }
;;

let data_to_json (data : t) : Json.t =
  `Assoc [
    ("iters", `Int data.iters);
    ("decay", `Float data.decay);
    ("margin", `Float data.margin);
    ("seed_wins", `List (List.map (fun x -> `Int x) data.seed_wins));
    ("is_fair", `Bool data.is_fair);
    ("max_games", `Int data.max_games)
  ]
;;

let calculate_imbalance (data : t) (fair_to_zero : bool) : float =
  if fair_to_zero && data.is_fair then 0. else Stats.normed_stdev (List.map Int.to_float data.seed_wins)
;;

let get_data_list ~(luck : float) ~(number_of_teams : int) ~(max_games : int) : (string * t) list =
  let lst = Json.read ~luck ~number_of_teams
    |> Json.to_object
    |> List.map (fun (name, json) -> name, json_to_data json)
    |> List.filter (fun (_, data) -> data.max_games <= max_games)
  in
  if List.length lst = 0 then begin
    print_endline "No such formats have been analyzed.";
    System.error ()
  end else
    lst
;;

(*merge with json.ml? (read/write etc)*)
let read_scheme_from_name (str : string) : Scheme.t =
  let rec f = function
    | number_of_teams :: "team" :: pool_count :: "pool" :: "format" :: "breaking" :: "to" :: "a" :: bracket ->
      Pool_play.make
        ~number_of_teams: (int_of_string number_of_teams)
        ~pool_count: (int_of_string pool_count)
        (f bracket)
    | _ :: "team" :: "bracket" :: "of" :: "shape" :: bracket ->
      let rec peel = function
        | [] -> []
        | wrd :: tl -> int_of_string (String.sub wrd 0 (String.length wrd - 1)) :: peel tl
    in Bracket.make (peel ((String.sub (List.hd bracket) 1 (String.length (List.hd bracket) - 1)) :: (List.tl bracket)))
    | _ -> System.error ()
  in f (String.split_on_char ' ' str)
;;