open! Util;;
open! Std;;

type t = Faithfulness.t = {
  number_of_pools : int;
  teams_per_pool : int;
  multibracket : Multibracket.t
} [@@deriving sexp];;

let kind = "pools";;

let number_of_teams t = t.number_of_pools * t.teams_per_pool;;

let run t game teams =
  teams
  |> List.outerleave ~rand:true t.number_of_pools
  |> List.map (Round_robin.run t.teams_per_pool game)
  |> List.map List.flatten
  |> List.interleave ~rand:true
  |> Multibracket.run t.multibracket game
;;

let get_all
    ?(respectfulness = Tier.Weakly)
    ?(triviality = Multibracket.Efficient)
    ?(faithful = true)
    ?(allow_two = false)
    ?(max_games = Int.max_int)
    (n : int)
    (prize : Prize.t)
  = 
  n
  |> Math.divisors
  |> List.filter (fun x -> x <> 2 || allow_two)
  |> List.map (fun x -> {number_of_pools = n / x; teams_per_pool = x; multibracket = []})
  |> List.map (Pair.join_right (fun t -> 
    t.teams_per_pool
    |> List.create 
    |> List.map (Fun.const Tier.{number_of_teams = t.number_of_pools; games_played = t.teams_per_pool - 1})
  ))
  |> List.map (Pair.map_right (fun tiers -> Multibracket.get_all ~respectfulness ~triviality ~max_games tiers prize))
  |> List.map (fun (t, m) -> List.map (fun multibracket -> {t with multibracket}) m)
  |> List.flatten
  |> Bool.do_if faithful (List.filter (fun f -> List.for_all (fun m -> Faithfulness.evaluate_format f m) prize))
;;




