type t = {
  scheme : Struct.Scheme.t;
  iters : int;
  decay : float;
  margin : float;
  seed_wins : int list;
}
val json_to_data : Util.Json.t -> t
val data_to_json : t -> Util.Json.t

val read : luck:float -> number_of_teams:int -> ?max_games:int -> bool -> t list
val write : luck:float -> number_of_teams:int -> t list -> unit
