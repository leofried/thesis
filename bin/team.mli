type t

val make : ?name:string -> ?skill:float -> unit -> t

val set_luck : float -> unit

val play_game : t -> t -> t * t

val get_skill : t -> float
val compare : t -> t -> int
