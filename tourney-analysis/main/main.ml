open Util;;
open Engine;;
open Struct;;

Rand.set_seed () ;;
print_endline "" ;;

module Data = Data.M (Scheme);;
module Simulate = Simulate.M (Scheme);;

let number_of_teams = 16;;

let specs = {(Specs.default number_of_teams) with
  number_advance = 1;
  max_games = 10;
  luck = 10.;
};;

Data.print specs;;
let schemes = Scheme.get_all specs;;
Simulate.simulate_smart_looped ~schemes specs 100000;;