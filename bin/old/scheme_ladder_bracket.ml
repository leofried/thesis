let rec run_ladder : Team.t list -> Team.t list = function
  | [] -> Util.error ()
  | t :: [] -> [t]
  | t1 :: ts ->
    match run_ladder ts with
    | [] -> Util.error ()
    | t2 :: ranks ->
      let winner, loser = Team.play_game t1 t2 in
      winner :: loser :: ranks
;;

let make (number_of_teams : int) : Scheme.t =
  Scheme.make_scheme "Ladder Bracket" number_of_teams run_ladder
;;