open Util;;
open Struct;;

exception NoSuchFormatsHaveBeen of string

module Scheme = struct
  include Scheme;;
  let t_of_yojson = t_of_yojson_helper [(module Round_robin); (module Bracket); (module Pool_play)];;
end

let path = ["analysis"; "specs"];;

let read (file_name : string) (throw : bool) : Scheme.t list =
  match Json.read path file_name with
  | None -> if throw then raise (NoSuchFormatsHaveBeen "Specified") else []
  | Some json -> Json.rip_list Scheme.t_of_yojson json
;;

let write ~(schemes : Scheme.t list) ~(file_name : string) ~(overwrite : bool) : unit =
  schemes
  |> List.append (if overwrite then [] else read file_name false)
  |> Json.place_list Scheme.yojson_of_t
  |> Json.write path file_name false
  ;
  print_endline (Int.to_string (List.length schemes) ^ " formats constructed.")
;;

