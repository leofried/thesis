val kind : string

val is_fair : int list -> int -> bool

val make : bracket:int list -> Scheme.t
val make_from_json : Util.Json.t -> Scheme.t

val get_all_brackets : int -> int list list list
