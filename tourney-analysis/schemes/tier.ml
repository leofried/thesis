open! Util

type t = {
  number_of_teams : int;
  games_played : int
}

let new_tier x = {
  number_of_teams = x;
  games_played = 0;
}

let number_of_teams t = t |> List.map (fun t -> t.number_of_teams) |> List.fold_left (+) 0

let play_game t = {
  number_of_teams = t.number_of_teams / 2;
  games_played = t.games_played + 1
}
                  
let merge t1 t2 = {
  number_of_teams = t1.number_of_teams + t2.number_of_teams;
  games_played = max t1.games_played t2.games_played
}

let split t n = (
  {
    number_of_teams = n;
    games_played = t.games_played
  }, 
  {
    number_of_teams = t.number_of_teams - n;
    games_played = t.games_played
  }
)
  
let rec run_bracket = function
  | [] -> assert false
  | [t] -> [t]
  | a :: b :: tl ->
      play_game a :: run_bracket (merge (play_game a) b :: tl)

type respectfulness =
  | Not
  | Weakly
    
let check_bracket (respectfulness : respectfulness) (tiers : t list) (bracket) =
  let rec f = function
    | [], tiers, [] -> Some (List.rev tiers, [])
    | so_far, tiers, 0 :: b_tl -> Option.map (Pair.map_right (List.cons (List.rev so_far))) (f ([], tiers, b_tl))
    | _, [], _ | _, _, [] -> assert false
    | so_far, t_hd :: t_tl, b_hd :: b_tl ->
        if t_hd.number_of_teams <= b_hd then
          f ((t_hd :: so_far), t_tl, ((b_hd - t_hd.number_of_teams) :: b_tl))
        else
          match respectfulness with
          | Not -> 
              let t1, t2 = split t_hd b_hd in
              f ((t1 :: so_far), (t2 :: t_tl), (0 :: b_tl))
          | Weakly -> None
  in
  f ([], List.rev tiers, List.rev bracket)
  |> Option.map (Pair.map_right List.rev)
  |> Option.map (Pair.map_right (List.map (List.fold_left merge (new_tier 0))))
  |> Option.map (Pair.map_right run_bracket)
  |> Option.map (Pair.uncurry List.append)