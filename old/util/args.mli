type getter = {
  _bool : string -> bool;
  _int : string -> int;
  _float : string -> float;
  _string : string -> string;
  _list : string -> int list;
}

type parameter =
    Bool of bool option
  | Int of int option
  | Float of float option
  | String of string option
  | List of int list option

type spec =
    Menu of (string * (string * parameter) list * spec) list
  | Final of (getter -> unit)

val parse : spec -> (getter -> unit) * getter
