module type S = sig
  type argument [@@deriving yojson]

  val kind : string
  val name : argument -> string
  val number_of_teams : argument -> int
  val max_games : argument -> int
  val is_fair : argument -> bool
  val run : argument -> Team.t list -> Team.t list
end

type _ s =
| Round_robin : (int * int) s
| Bracket : (string) s
;;

type t = Format : 'a s * 'a -> t ;;

let modulize : type a . a s -> (module S with type argument = a) = function
  | Round_robin -> (module Round_robin : S with type argument = a)
  | Bracket -> (module Bracket : S with type argument = a)
;;

let name : t -> string = fun (Format (kind, arg)) ->
  let module M = (val (modulize kind) : S with type argument = 'a) in
  M.name arg
;;

let number_of_teams : t -> int = fun (Format (kind, arg)) ->
  let module M = (val (modulize kind) : S with type argument = 'a) in
  M.number_of_teams arg
;;

let max_games : t -> int = fun (Format (kind, arg)) ->
  let module M = (val (modulize kind) : S with type argument = 'a) in
  M.max_games arg
;;

let is_fair : t -> bool = fun (Format (kind, arg)) ->
  let module M = (val (modulize kind) : S with type argument = 'a) in
  M.is_fair arg
;;

let run : t -> Team.t list -> Team.t list = fun (Format (kind, arg)) ->
  let module M = (val (modulize kind) : S with type argument = 'a) in
  M.run arg
;;

let kind_s : type a. a s -> string = fun kind ->
  let module M = (val (modulize kind) : S with type argument = a) in
  M.kind
;;

let kind_t : t -> string = fun (Format (kind, _)) ->
  let module M = (val (modulize kind) : S with type argument = 'a) in
  M.kind
;;

let to_json : t -> Json.t = fun (Format (kind, arg)) ->
  let module M = (val (modulize kind) : S with type argument = 'a) in
  `Assoc [
    ("kind", `String M.kind);
    ("args", M.yojson_of_argument arg)
  ]
;;

let of_json : Json.t -> t = fun json ->
  let args = Json.member "args" json in
  match Json.(rip to_string "kind" json) with
    | "round_robin" -> Format (Round_robin, Round_robin.argument_of_yojson args)
    | "bracket" -> Format (Bracket, Bracket.argument_of_yojson args)
    | _ -> assert false
;;