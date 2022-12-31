val kind : string

val make : number_of_teams:int -> pool_count:int -> bracket:int list -> Scheme.t
val make_from_json : Util.Json.t -> Scheme.t

val get_all_pools :
  number_of_teams:int ->
  pool_counts:int list -> max_games:int -> Scheme.t list
