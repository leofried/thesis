open! Util;;
open! Std;;

type t = {
  number_of_pools : int;
  teams_per_pool : int;
  multibracket : Multibracket.t
} [@@deriving sexp]

let kind = "pools";;

let number_of_teams t = t.number_of_pools * t.teams_per_pool;;

let run _ = assert false;;




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




