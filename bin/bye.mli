type t

val play_game : t -> t -> t * t

val is_bye : t -> bool
val get_team : t -> Team.t

val fill: Team.t list -> t list
val empty : t list -> Team.t list

