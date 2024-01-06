open! Util


type t = {
  number_of_teams : int;
  games_played : int
} 
          
type respectfulness =
  | Not
  | Weakly
  | Strongly

let new_tier x = {
  number_of_teams = x;
  games_played = 0;
}

let number_of_teams t = t |> List.map (fun t -> t.number_of_teams) |> List.fold_left (+) 0

let merge = List.fold_left (fun t1 t2 -> {
      number_of_teams = t1.number_of_teams + t2.number_of_teams;
      games_played = max t1.games_played t2.games_played
    }) (new_tier 0)     
    
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

let play_game lst = function
  | Not | Weakly ->
      lst
      |> merge
      |> (fun t -> {
            number_of_teams = t.number_of_teams / 2;
            games_played = t.games_played + 1
          })
      |> Pair.join_right List.item
      |> Option.some
  | Strongly ->
      let bool, lst =
        lst
        |> List.combine (List.rev lst)
        |> List.fold_left_map (fun b (t1, t2) ->
            b && t1.number_of_teams = t2.number_of_teams, {
              number_of_teams = t1.number_of_teams;
              games_played = 1 + max t1.games_played t2.games_played
            }) true
      in if not bool then 
        None 
      else let lst =
             match List.length lst mod 2 with
             | 0 -> 
                 lst
                 |> List.top_of_list (List.length lst / 2)
                 |> Pair.right
             | _ -> 
                 lst
                 |> List.top_of_list (List.length lst / 2)
                 |> Pair.right
                 |> List.on_loc 0 (fun t -> split t (t.number_of_teams / 2) |> Pair.right)
        in Some (merge lst, lst) 

let rec run_bracket (respectfulness : respectfulness) = function
  | [] -> assert false
  | [t] -> Some ([merge t])
  | a :: b :: tl ->
      play_game a respectfulness
      |> Option.bind (fun (losers, winners) -> 
          Option.map (List.cons losers) (run_bracket respectfulness ((winners @ b) :: tl)) (*fix gross options*)
        )
  
let check_bracket (respectfulness : respectfulness) (tiers : t list) (bracket) =
  let rec f = function
    | [], tiers, [] -> Some (List.rev tiers, [])
    | so_far, tiers, 0 :: b_tl -> Option.map (Pair.map_right (List.cons so_far)) (f ([], tiers, b_tl))
    | _, [], _ | _, _, [] -> assert false
    | so_far, t_hd :: t_tl, b_hd :: b_tl ->
        if t_hd.number_of_teams <= b_hd then
          f ((t_hd :: so_far), t_tl, ((b_hd - t_hd.number_of_teams) :: b_tl))
        else
          match respectfulness with
          | Not -> 
              let t1, t2 = split t_hd b_hd in
              f ((t1 :: so_far), (t2 :: t_tl), (0 :: b_tl))
          | Weakly | Strongly -> None 
  in
  f ([], List.rev tiers, List.rev bracket)
  |> Option.map (Pair.map_right List.rev)
  |> Option.map (Pair.map_right (run_bracket respectfulness))
  |> Option.map (Pair.uncurry Pair.tuck)
  |> Option.join
  |> Option.map (Pair.uncurry List.append) 
;;
