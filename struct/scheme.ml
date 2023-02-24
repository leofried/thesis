open Util;;

module type S = sig
  type argument [@@deriving yojson]

  val kind : string
  val name : argument -> string
  val number_of_teams : argument -> int
  val is_fair : argument -> bool
  val run : argument -> Team.t list -> Team.t list

(*  val get_all : number_of_teams : int -> max_games : int -> argument list *)
end

type 'a s = (module S with type argument = 'a);;

type t = Format : 'a s * 'a -> t;;

let name (Format (kind, arg)) : string =
  let module M = (val kind : S with type argument = 'a) in
  M.name arg
;;

let number_of_teams (Format (kind, arg)) : int = 
  let module M = (val kind : S with type argument = 'a) in
  M.number_of_teams arg
;;

let is_fair (Format (kind, arg)) : bool =
   let module M = (val kind : S with type argument = 'a) in
  M.is_fair arg
;;

let run (Format (kind, arg)) : Team.t list -> Team.t list =
  let module M = (val kind : S with type argument = 'a) in
  M.run arg
;;

let kind_s : type a. a s -> string = fun kind ->
  let module M = (val kind : S with type argument = a) in
  M.kind
;;

let kind_t (Format (kind, _)) : string =
  kind_s kind
;;

(*let get_all : type a. a s -> number_of_teams : int -> max_games : int -> t list = fun kind ~number_of_teams ~max_games ->
  let module M = (val kind : S with type argument = a) in
 let _ = List.map (fun arg -> Format(kind, arg)) (M.get_all ~number_of_teams ~max_games)
  in assert false;
;;*)

let max_games (Format (kind, arg)) : int = 
  let module M = (val kind : S with type argument = 'a) in
  M.number_of_teams arg
  |> Team.make_n
  |> M.run arg
  |> List.fold_left (fun n t -> max n t.Team.games) 0
;;

let yojson_of_t (Format (kind, arg)) : Json.t =
  let module M = (val kind : S with type argument = 'a) in
  `Assoc [
    ("kind", `String M.kind);
    ("args", M.yojson_of_argument arg)
  ]
;;

let t_of_yojson_helper (lst : (module S) list) (json : Json.t) : t =
  let kind = Json.(rip_member to_string "kind" json) in
  let rec f = function
    | [] -> assert false
    | hd :: tl ->
      let module M = (val hd : S) in
      if M.kind = kind then
        Format ((module M : S with type argument = M.argument), M.argument_of_yojson (Json.member "args" json))
      else f tl
  in f lst
;;

let equals (t1 : t) (t2 : t) : bool = yojson_of_t t1 = yojson_of_t t2;;