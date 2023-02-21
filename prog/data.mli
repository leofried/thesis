type t = {
  scheme : Struct.Scheme.t;
  iters : int;
  decay : float;
  margin : float;
  seed_wins : int list;
}

val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t


val read : luck:float -> number_of_teams:int -> ?max_games:int -> bool -> t list
val write : luck:float -> number_of_teams:int -> t list -> unit
