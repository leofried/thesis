open! Util;;
open! Std;;

type t = {
  number_of_pools : int;
  teams_per_pool : int;
  multibracket : Multibracket.t
} [@@deriving sexp]

let kind = "pools";;

let number_of_teams t = t.number_of_pools * t.teams_per_pool;;

let run t play teams =
  teams
  |> List.outerleave ~rand:true t.number_of_pools
  |> List.map (Round_robin.run t.teams_per_pool play)
  |> List.map List.flatten
  |> List.interleave ~rand:true
  |> Multibracket.run t.multibracket play
;;
  
(*   
  
  match t with
| [] -> [teams]
| hd :: tl ->
  let n = Proper.number_of_teams hd in
  let top, bot = List.top_of_list n teams in
  let results = Proper.run hd play top in
  List.hd results :: run tl play (List.flatten (List.tl results) @ bot)
;; *)




let get_all
    ?(respectfulness = Tier.Weakly)
    ?(triviality = Multibracket.Efficient)
    ?(max_games = Int.max_int)
    (n : int)
    (output_tiers : int list)
  = 
  n
  |> Math.divisors
  |> List.map (fun x -> {number_of_pools = x; teams_per_pool = n / x; multibracket = []})
  |> List.map (Pair.join_right (fun t -> 
    t.teams_per_pool
    |> List.create 
    |> List.map (Fun.const Tier.{number_of_teams = t.number_of_pools; games_played = t.teams_per_pool - 1})
  ))
  |> List.map (Pair.map_right (fun input_tiers -> Multibracket.get_all ~respectfulness ~triviality ~max_games input_tiers output_tiers))
  |> List.map (fun (t, m) -> List.map (fun multibracket -> {t with multibracket}) m)
  |> List.flatten
;;




