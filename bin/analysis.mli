val analyze_scheme : luck:float -> iters:int -> Scheme.t -> unit

val analyze_schemes : luck:float -> iters:int -> Scheme.t list -> unit

val pareto_report : luck:float -> number_of_teams:int -> max_games:int -> unit