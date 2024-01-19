open! Util
open! Std

type t = {
  number_of_pools : int;
  teams_per_pool : int;
  multibracket : Multibracket.t
} [@@deriving sexp]

type team = Good | Bad;;

let sort_teams = List.sort (fun x y -> if x = y then 0 else if x = Good then -1 else 1);; 

let simulate_round (teams : team list) =
  teams
  |> sort_teams
  |> List.top_of_list (List.length teams / 2)
;; 

let simulate_bracket (teams : team list) (bracket : Proper.t) = 
  let rec f teams = function
    | [] -> [], teams
    | hd :: tl ->
        let byes, players_bottom = List.top_of_list hd teams in
        let winners_bottom, losers_bottom = f players_bottom tl in
        let winners_top, losers_top = simulate_round winners_bottom in 
        byes @ winners_top, losers_top @ losers_bottom 
  in f teams (List.rev bracket)
;;


let rec simulate_multibracket (teams : team list) = function
  | [] -> teams
  | hd :: tl ->
      let winners, losers = simulate_bracket teams hd in
      winners @ simulate_multibracket losers tl
;;

let evaluate_multibracket (multi : Multibracket.t) (teams : team list) =
  simulate_multibracket teams multi = sort_teams teams
;; 
  
let evaluate_format (pools : t) (m : int) =
  Combo.partitions ~n:m ~m:pools.teams_per_pool ~l:pools.number_of_pools ()
  |> List.map (List.map (fun x ->
    pools.teams_per_pool
    |> List.create
    |> List.map (fun i -> if i < x then Good else Bad)
  ))
  |> List.map List.interleave
  |> List.for_all (evaluate_multibracket pools.multibracket)
;; 