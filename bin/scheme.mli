type t

val make_scheme : string -> int -> (Team.t list -> Team.t list) -> t

val to_string : t -> string

val number_of_teams : t -> int

val run : t -> Team.t list -> Team.t list

