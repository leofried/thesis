open! Util;;
open! Std;;

type t = Faithfulness.t = {
  number_of_pools : int;
  teams_per_pool : int;
  cycles_per_pool : int;
  multibracket : Multibracket.t
} [@@deriving sexp];;

let kind = "pools";;

let number_of_teams t = t.number_of_pools * t.teams_per_pool;;

let run t game teams =
  teams
  |> List.outerleave ~rand:true t.number_of_pools
  |> List.map (Round_robin.run {number_of_teams = t.teams_per_pool; cycles = t.cycles_per_pool} game)
  |> List.map List.flatten
  |> List.interleave ~rand:true
  |> Multibracket.run t.multibracket game
;;

let get_all
    ?(respectfulness = Tier.Weakly)
    ?(triviality = Multibracket.Efficient)
    ?(faithful = true)
    ?(allow_one = true)
    ?(allow_two = false)
    ?(allow_cycles = false)
    ?(max_games)
    (n : int)
    (prize : Prize.t)
  = 
  n
  |> Math.divisors
  |> Bool.do_if_not allow_one (List.filter (fun x -> x <> 1))
  |> Bool.do_if_not allow_two (List.filter (fun x -> x <> 2))
  |> List.map (fun x -> 
    match allow_cycles, x, max_games with
    | false, _, _ | _, 1, _ | _, _, None -> [{number_of_pools = n / x; teams_per_pool = x; cycles_per_pool = 1; multibracket = []}]
    | _, _, Some m -> (m / (x - 1)) |> List.create |> List.map (fun c -> {number_of_pools = n / x; teams_per_pool = x; cycles_per_pool = (c + 1); multibracket = []})
    )
  |> List.flatten
  |> List.map (Pair.join_right (fun t -> 
    t.teams_per_pool
    |> List.create 
    |> List.map (Fun.const Tier.{number_of_teams = t.number_of_pools; games_played = t.cycles_per_pool * (t.teams_per_pool - 1)})
  ))
  |> List.map (Pair.map_right (fun tiers -> Multibracket.get_all ~respectfulness ~triviality ?max_games tiers prize))
  |> List.map (fun (t, m) -> List.map (fun multibracket -> {t with multibracket}) m)
  |> List.flatten
  |> Bool.do_if faithful (List.filter (fun f -> List.for_all (fun m -> Faithfulness.evaluate_format f m) prize))
;;




