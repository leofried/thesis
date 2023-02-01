module type S = sig
  type argument

  val name : argument -> string
  val number_of_teams : argument -> int
  val max_games : argument -> int
  val is_fair : argument -> bool
  val run : argument -> Team.t list -> Team.t list

  type one_argument
  val kind : string
  val params : (argument, one_argument) Param_specs.t
end

(*scheme vs format*)

type (_, _) s =
| Round_robin : (int * int, (Param_specs.one_int * Param_specs.one_int)) s
(*| Bracket : int list s*)
;;

type t = Format : ('a, 'b) s * 'a -> t;;

let modulize_one : type a b. (a, b) s -> (module S with type argument = a and type one_argument = b) = function
  | Round_robin -> (module Round_robin : S with type argument = a and type one_argument = b)
(*  | Bracket -> (module Bracket : S with type argument = a)*)
;;

let modulize : type a b. (a, b) s -> (module S with type argument = a) = fun x ->
  let module M = (val (modulize_one x) : S with type argument = a and type one_argument = b) in
  (module M : S with type argument = a)
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

let kind : type a b. (a, b) s -> string = fun kind ->
  let module M = (val (modulize_one kind) : S with type argument = a and type one_argument = b) in
  M.kind
;;

let params : type a b. (a, b) s -> (a, b) Param_specs.t = fun kind ->
  let module M = (val (modulize_one kind) : S with type argument = a and type one_argument = b) in
  M.params
;;



(*
  

  let modulize : type a. a s -> (module S with type argument = a) = function
  | Round_robin -> (module Round_robin : S with type argument = a)
  | Bracket -> (module Bracket : S with type argument = a)
;;

let unpack : type a. a t -> a = function
  | Round_robin (x, y) -> (x, y)
  | Bracket x -> x
;;

let zoom : type a. a t -> a s = function
  | Round_robin (_, _) -> Round_robin
  | Bracket (_) -> Bracket
;;
  
  function
  | Round_robin (number_of_teams, _) -> number_of_teams
  | Bracket bracket -> List.fold_left ( + ) 0 bracket
;;

let name = function
  | Round_robin (number_of_teams, cycles) -> Int.to_string number_of_teams ^ " team " ^ Int.to_string cycles ^ "-Round Robin"
  | Bracket bracket -> Int.to_string (number_of_teams (Bracket bracket)) ^ " team " ^ Lists.to_string Int.to_string bracket ^ "-bracket"
;;

let max_games = function
  | Round_robin (number_of_teams, cycles) -> (number_of_teams - 1) * cycles
  | Bracket bracket -> Bracket.count_games bracket
;;

let is_fair = function
  | Round_robin (_, _) -> true
  | Bracket bracket -> Bracket.is_fair number_of_teams
*)
(*
type t = {
  name : string;
  number_of_teams : int;
  max_games : int;
  is_fair : bool;
  run : Team.t list -> Team.t list;
}

type ('a, 'b) eliom_builder = {
  name : string;
  params : ('a, 'b) Param_specs.t;
  make : 'a -> t
}*)