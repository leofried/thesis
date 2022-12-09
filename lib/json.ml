type t = Yojson.Basic.t;;

let json_file_name ~(number_of_teams : int) ~(luck : float) : string =
  "analysis/teams_" ^ Int.to_string number_of_teams ^ "_luck_" ^ Float.to_string luck ^ ".json"
;;

let read ~(luck : float) ~(number_of_teams : int) : t =
  let file_name = json_file_name ~luck ~number_of_teams in
  if Sys.file_exists file_name then
    Yojson.Basic.from_file file_name
  else
    `Assoc []
;;

let write ~(luck : float) ~(number_of_teams : int) (json : t) : unit =
  if not @@ Sys.file_exists "analysis/" then Sys.mkdir "analysis/" 0;
  let channel = open_out (json_file_name ~luck ~number_of_teams) in
  Yojson.Basic.pretty_to_channel channel json;
  flush channel
;;

let member = Yojson.Basic.Util.member

let combine = Yojson.Basic.Util.combine

let has_key (key : string) (json : t) : bool =
  member key json <> `Null
;;

let rip_int (key : string) (json : t) : int =
  Yojson.Basic.Util.to_int (member key json)
;;

let rip_float (key : string) (json : t) : float =
  Yojson.Basic.Util.to_float (member key json)
;;

let rip_int_list (key : string) (json : t) : int list =
  Yojson.Basic.Util.convert_each Yojson.Basic.Util.to_int (member key json)
;;

let overwrite_key (key : string) (value : t) (json : t) : t =
  json
  |> Yojson.Basic.Util.to_assoc
  |> List.map (fun (jkey, jvalue) -> (jkey, if jkey = key then value else jvalue))
  |> (fun x -> `Assoc x)