let build_matchups (teams : Bye.t list) : (Bye.t * Bye.t) list =
  let size = (List.length teams) / 2 in
  let upper, lower = Util.apply (fun (upper, lower) -> (List.hd lower :: upper), (List.tl lower)) size ([], teams) in
  List.combine upper lower
;;

let play_matchups : (Bye.t * Bye.t) list -> (Bye.t list * Bye.t list) =
  List.fold_left (
    fun (alive, dead) (o1, o2) ->
      let winner, loser = Bye.play_game o1 o2 in
      winner :: alive, loser :: dead      
  ) ([], [])
;;

let rec run_full_bracket (teams : Bye.t list) : Bye.t list =
  match List.length teams with
  | 1 -> [List.hd teams]
  | _ -> 
    let matchups = build_matchups teams in
    let alive, dead = play_matchups matchups in
    (run_full_bracket alive) @ dead
;;

let run_bracket (teams : Team.t list) : Team.t list = 
  teams
  |> Bye.fill
  |> run_full_bracket
  |> Bye.empty
;;

let make (number_of_teams : int) : Scheme.t =
  Scheme.make_scheme "Simple Bracket" number_of_teams run_bracket
;;



  
