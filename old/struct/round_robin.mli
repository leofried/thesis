val kind : string

val make : cycles:int -> number_of_teams:int -> Scheme.t
val make_from_json : Util.Json.t -> Scheme.t
