(*val calculate_imbalance : Data.t -> bool -> float

val get_pareto_list :
  luck:float ->
  number_of_teams:int ->
  max_games:int -> (Struct.Scheme.t * float * float) list
  
val pareto : luck:float -> number_of_teams:int -> max_games:int -> unit
val all : luck:float -> number_of_teams:int -> max_games:int -> unit
*)