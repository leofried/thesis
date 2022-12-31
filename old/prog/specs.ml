open Util;;
open Struct;;

exception NoSuchFormatsHaveBeen of string

let path = ["analysis"; "specs"];;

let json_to_scheme (json : Json.t) : Scheme.t =
  let kind = Json.rip Json.to_string "kind" json in
  let rec f = function
    | [] -> raise (NoSuchFormatsHaveBeen "Defined")
    | hd :: tl ->
      let module Mod = (val hd : Scheme.KIND) in
      if Mod.kind = kind then Mod.make_from_json json else f tl
 in f [(module Round_robin); (module Bracket); (module Pool_play)]
;;

let read (file_name : string) (throw : bool) : Scheme.t list =
  match Json.read path file_name with
  | None -> if throw then raise (NoSuchFormatsHaveBeen "Specified") else []
  | Some json -> List.map json_to_scheme (Json.to_list json)
;;

let write ~(schemes : Scheme.t list) ~(file_name : string) ~(overwrite : bool) : unit =
  schemes
  |> List.append (if overwrite then [] else read file_name false)
  |> List.map (fun (x : Scheme.t) -> x.json)
  |> (fun x -> `List x)
  |> Json.write path file_name
  ;
  print_endline (Int.to_string (List.length schemes) ^ " formats constructed.")
;;

