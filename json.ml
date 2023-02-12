type t = Yojson.Safe.t;;

let read (folders : string list)(file_name : string) : t option =
  let path = List.fold_right (fun fld str -> fld ^ "/" ^ str) folders (file_name ^ ".json") in
  if Sys.file_exists path then
    Some (Yojson.Safe.from_file path)
  else
    None
;;

let write (folders : string list) (file_name : string) (backup : bool) (json : t) : unit =
  let rec f lst path =
    match lst with
    | [] -> path
    | hd :: tl ->
      let new_path = path ^ hd ^ "/" in
      if not @@ Sys.file_exists new_path then Sys.mkdir new_path 0;
      f tl new_path
  in let g file_name =
    let path = (f folders "") ^ file_name ^ ".json" in
    let channel = open_out path in
    Yojson.Safe.pretty_to_channel channel json;
    flush channel;
  in
  g file_name;
  if backup then g (file_name ^ "_backup") else ();
;;

let member = Yojson.Safe.Util.member;;

let has_key (key : string) (json : t) = member key json <> `Null;;

let to_bool = Yojson.Safe.Util.to_bool;;
let to_int = Yojson.Safe.Util.to_int;;
let to_float = Yojson.Safe.Util.to_float;;
let to_string = Yojson.Safe.Util.to_string;;
let to_assoc = Yojson.Safe.Util.to_assoc;;
let to_list = Yojson.Safe.Util.to_list;;

let rip_member (f : t -> 'a) (key : string) (json : t) = f (member key json);;

let rip_list (f : t -> 'a) (json : t) = List.map f (to_list json);;

let place_list (f : 'a -> t) (lst : 'a list) : t = `List (List.map f lst)


type u =
  | Null
  | Bool of bool
  | Int of int
  | Intlit of string
  | Float of float
  | String of string
  | Assoc of (string * u) list
  | List of u list
  | Tuple of u list
  | Variant of (string * u option)
  (**)
  [@@deriving json]
;;

let rec t_of_u : u -> t = function
  | Null -> `Null
  | Bool b -> `Bool b
  | Int i -> `Int i
  | Intlit s -> `Intlit s
  | Float f -> `Float f
  | String s -> `String s
  | Assoc lst -> `Assoc (List.map (fun (s, u) -> (s, t_of_u u)) lst)
  | List lst -> `List (List.map t_of_u lst)
  | Tuple lst -> `Tuple (List.map t_of_u lst)
  | Variant (s, o) -> `Variant (s, Option.map t_of_u o)
;;

let rec u_of_t : t -> u = function
  | `Null -> Null
  | `Bool b -> Bool b
  | `Int i -> Int i
  | `Intlit s -> Intlit s
  | `Float f -> Float f
  | `String s -> String s
  | `Assoc lst -> Assoc (List.map (fun (s, u) -> (s, u_of_t u)) lst)
  | `List lst -> List (List.map u_of_t lst)
  | `Tuple lst -> Tuple (List.map u_of_t lst)
  | `Variant (s, o) -> Variant (s, Option.map u_of_t o)
;;

let wrap f x = u_of_t (f (t_of_u x));;