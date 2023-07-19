open! Util
open! Schemes
open! Metrics
open! Engine


let number_of_teams = 4;;
let specs = {Specs.default with number_of_teams};;
let metrics =
  number_of_teams + 1
  |> List.create
  |> List.map (fun x -> "decay", Decay.sexp_of_t x)
  |> List.map (fun m -> Metric.empty m)
;;
let schemes = [
  "round_robin", Round_robin.sexp_of_t number_of_teams;
  "tof_bracket", Tof_bracket.sexp_of_t Balanced;
  "tof_bracket", Tof_bracket.sexp_of_t Ladder;
];;

schemes
|> List.map (fun scheme -> scheme, 800000, metrics)
|> Simulate.simulate_schemes ~specs
|> Data.write ~specs
;;

number_of_teams + 1
|> List.create
|> List.map (fun x -> "decay", Decay.sexp_of_t x)
|> List.iter (fun x -> print_endline "..."; Data.print ~specs x)
;;
