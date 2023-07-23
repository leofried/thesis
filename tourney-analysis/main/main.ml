open! Util
open! Schemes
open! Metrics
open! Engine

let number_of_teams = 4;;
let specs = {Specs.default with number_of_teams};;
let metrics =
  number_of_teams + 1
  |> List.create
  |> List.map (Metric.create (module Decay))
;;
let schemes = [
  Scheme.create (module Round_robin) number_of_teams;
  Scheme.create (module Tof_bracket) Balanced;
  Scheme.create (module Tof_bracket) Ladder;
];;

schemes
|> List.map (fun scheme -> scheme, 80000, metrics)
|> Simulate.simulate_schemes ~specs
|> Data.write ~specs
;;

number_of_teams + 1
|> List.create
|> List.map (fun x -> "decay", Decay.sexp_of_t x)
|> List.iter (fun x -> print_endline "..."; Data.print ~specs x)
;;
