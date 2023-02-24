type t 

val make : ?name:string -> ?skill:float -> unit -> t

val make_n : int -> t list

val set_luck : float -> unit

val play_game : t -> t -> bool -> t * t

val get_skill : t -> float

val max_games : t list -> int
