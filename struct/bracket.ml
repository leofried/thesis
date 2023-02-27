open Util;;
open Infix;;

type argument = int list [@@deriving yojson];; (*validate that a given int list is a bracket*)

let kind = "bracket";;

let number_of_teams (bracket : argument) = Lists.fold (+) bracket;;

let name (bracket : argument) = Int.to_string (number_of_teams bracket) ^ " team " ^ Lists.to_string Int.to_string (bracket) ^ "-bracket";;

(*let is_fair (bracket : argument) = 
  let rec f  (bracket : argument) (seed_size : int) : bool =
    match bracket with
    | [] -> true
    | hd :: tl -> if hd mod seed_size = 0 then f tl seed_size else false
  in f bracket (number_of_teams bracket)
;;*)

let run (bracket : argument) : Team.t list -> Team.t list =
  let rec run_reversed (bracket : argument) (teams : Team.t list) = (*only reverse the top X*)
    match bracket with
    | [] -> assert false
    | [_] -> teams
    | hd :: md :: tl ->
      let n = hd / 2 in
      let dead, alive =
        teams
        |> Lists.top_of_list_rev n
        |> Tuple.map_right (Lists.top_of_list n)
        |> Tuple.associate_left
        |> Tuple.map_left (
          Tuple.uncurry List.combine
          >> List.map (Tuple.uncurry (Team.play_game true))
          >> List.split
          >> Tuple.commute
        )
        |> Tuple.associate_right
        |> Tuple.map_right (Tuple.uncurry List.append)
      in
      (run_reversed ((md + n) :: tl) alive) @ dead
  in
  Lists.top_of_list (number_of_teams bracket)
  >> Tuple.map_left (
    List.rev
    >> run_reversed bracket
  )
  >> Tuple.uncurry List.append
;;