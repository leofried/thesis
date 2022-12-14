open Util;;

type t = {
  name : string;
  number_of_teams : int;
  max_games : int;
  is_fair : bool;
  run : Team.t list -> Team.t list;
  json : Json.t;
};;

let kind_key = "kind";;

module type kind = sig
  val kind : string
  val make_from_json : Json.t -> t
end