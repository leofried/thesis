type t = Yojson.Safe.t;;

let member = Yojson.Safe.Util.member;;

let has_key (key : string) (json : t) = member key json <> `Null;;


let to_bool = Yojson.Safe.Util.to_bool;;
let to_int = Yojson.Safe.Util.to_int;;
let to_float = Yojson.Safe.Util.to_float;;
let to_string = Yojson.Safe.Util.to_string;;
let to_assoc = Yojson.Safe.Util.to_assoc;;
let to_list = Yojson.Safe.Util.to_list;;

let rip (f : t -> 'a) (key : string) (json : t) = f (member key json);;

let rip_list (f : t -> 'a) (key : string) (json : t) = List.map f (rip to_list key json)

let place_int_list (lst : int list) : t = `List (List.map (fun x -> `Int x) lst);;