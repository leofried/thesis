open Util;;
open Engine;;
open Struct;;

Rand.set_seed () ;;
print_endline "" ;;

module Scheme = Roc;;

module Data = Data.M (Scheme);;
module Simulate = Simulate.M (Scheme);;

let number_of_teams = 24;;
let number_advance = 1;;
let max_games = 8;;
let specs = {(Data.default_specs number_of_teams) with number_advance; max_games};;

Data.print specs;;
let all = Scheme.Round_robin :: (Scheme.Bracket [1]) :: Sptb.build_all specs;;
print_int (List.length all);;
Simulate.simulate_schemes specs 1000 all;;
Simulate.simulate_smart_looped specs 100_000;;