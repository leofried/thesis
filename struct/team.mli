type t = private {name : string; skill : float; mutable games : int}

val make : ?name:string -> ?skill:float -> unit -> t

val make_n : int -> t list

val set_luck : float -> unit

val play_game : bool -> t -> t -> t * t
