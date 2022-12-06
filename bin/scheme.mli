type t

val make_scheme : string -> int -> int -> (int -> bool) -> (Team.t list -> Team.t list) -> t

val to_string : t -> string

val number_of_teams : t -> int

val max_games : t -> int

val is_fair : t -> int -> bool

val run : t -> Team.t list -> Team.t list


