type t = {name : string; number_of_teams : int; run : Team.t list -> Team.t list};;

let make_scheme (name : string) (number_of_teams : int) (run : Team.t list -> Team.t list) : t =
  {name; number_of_teams; run};;

let number_of_teams (scheme : t) : int = scheme.number_of_teams;;

let verify_number_of_teams (scheme : t) (teams : Team.t list) =
  if (List.length teams) != scheme.number_of_teams then Util.error ()
;;

let run (scheme: t) (teams : Team.t list) : Team.t list =
  verify_number_of_teams scheme teams;
  let ranks = scheme.run teams in
  verify_number_of_teams scheme ranks;
  ranks
;;

let to_string (scheme : t) = (Int.to_string scheme.number_of_teams) ^ " team " ^ scheme.name;;