type t = Yojson.Basic.t;;

let data_file_name ~(number_of_teams : int) ~(luck : float) : string =
  "analysis/data/teams_" ^ Int.to_string number_of_teams ^ "_luck_" ^ Float.to_string (Float.round (luck *. 100.)) ^ "json"
;;

let read_analysis ~(luck : float) ~(number_of_teams : int) : t =
  let file_name = data_file_name ~luck ~number_of_teams in
  if Sys.file_exists file_name then
    Yojson.Basic.from_file file_name
  else
    `List []
;;

let write_analysis ~(luck : float) ~(number_of_teams : int) (json : t) : unit =
  if not @@ Sys.file_exists "analysis/" then Sys.mkdir "analysis/" 0;
  if not @@ Sys.file_exists "analysis/data/" then Sys.mkdir "analysis/data/" 0;
  let channel = open_out (data_file_name ~luck ~number_of_teams) in
  Yojson.Basic.pretty_to_channel channel json;
  flush channel
;;

let specs_file_name ~(name : string) : string =
  "analysis/specs/" ^ name ^ ".json"
;;

let read_specs ~(name : string) : t =
  let file_name = specs_file_name ~name in
  if Sys.file_exists file_name then
    Yojson.Basic.from_file file_name
  else
    let () = print_endline "No such formats specified." in System.error ()
;;

let write_specs ~(name : string) (json : t) : unit =
  if not @@ Sys.file_exists "analysis/" then Sys.mkdir "analysis/" 0;
  if not @@ Sys.file_exists "analysis/specs/" then Sys.mkdir "analysis/specs/" 0;
  let channel = open_out (specs_file_name ~name ) in
  Yojson.Basic.pretty_to_channel channel json;
  flush channel
;;

let member = Yojson.Basic.Util.member;;

let to_list = Yojson.Basic.Util.to_list;;

let has_key (key : string) (json : t) : bool =
  member key json <> `Null
;;

let rip_int (key : string) (json : t) : int =
  Yojson.Basic.Util.to_int (member key json)
;;

let rip_float (key : string) (json : t) : float =
  Yojson.Basic.Util.to_float (member key json)
;;

let rip_bool (key : string) (json : t) : bool =
  Yojson.Basic.Util.to_bool (member key json)
;;

let rip_string (key : string) (json : t) : string =
  Yojson.Basic.Util.to_string (member key json)
;;


let rip_list (key : string) (json : t) : int list =
  Yojson.Basic.Util.convert_each Yojson.Basic.Util.to_int (member key json)
;;

let place_list (lst : int list) : t =
  `List (List.map (fun x -> `Int x) lst)
;;

let set_key (key : string) (value : t) (json : t) : t =
  if has_key key json then
    json
    |> Yojson.Basic.Util.to_assoc
    |> List.map (fun (jkey, jvalue) -> (jkey, if jkey = key then value else jvalue))
    |> (fun x -> `Assoc x)
  else
    Yojson.Basic.Util.combine json (`Assoc [key, value])
;;
  