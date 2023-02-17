type t = Yojson.Basic.t;;

let read (folders : string list)(file_name : string) : t option =
  let path = List.fold_right (fun fld str -> fld ^ "/" ^ str) folders (file_name ^ ".json") in
  if Sys.file_exists path then
    Some (Yojson.Basic.from_file path)
  else
    None
;;

let write (folders : string list) (file_name : string) (json : t) : unit =
  let rec f lst path =
    match lst with
    | [] -> path
    | hd :: tl ->
      let new_path = path ^ hd ^ "/" in
      if not @@ Sys.file_exists new_path then Sys.mkdir new_path 0;
      f tl new_path
  in
  let path = (f folders "") ^ file_name ^ ".json" in
  let channel = open_out path in
  Yojson.Basic.pretty_to_channel channel json;
  flush channel
;;

let member = Yojson.Basic.Util.member;;

let has_key (key : string) (json : t) = member key json <> `Null;;


let to_bool = Yojson.Basic.Util.to_bool;;
let to_int = Yojson.Basic.Util.to_int;;
let to_float = Yojson.Basic.Util.to_float;;
let to_string = Yojson.Basic.Util.to_string;;
let to_assoc = Yojson.Basic.Util.to_assoc;;
let to_list = Yojson.Basic.Util.to_list;;

let rip (f : t -> 'a) (key : string) (json : t) = f (member key json);;

let rip_list (f : t -> 'a) (key : string) (json : t) = List.map f (rip to_list key json)

let place_int_list (lst : int list) : t = `List (List.map (fun x -> `Int x) lst);;