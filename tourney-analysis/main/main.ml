open! Util
open! Schemes
open! Metrics
open! Engine

let number_of_teams = 4;;
let fidel = 1.;;
let specs = {Specs.default with number_of_teams; fidel};;
let metrics = Metric.empty (module Disorder) number_of_teams :: (
  number_of_teams + 1
  |> List.create
  |> List.map (Metric.empty (module Decay))
);;

let schemes = Scheme.create (module Round_robin) number_of_teams :: (
  number_of_teams
  |> Bracket.get_all
  |> List.map (Scheme.create (module Bracket))
);;



(*number_of_teams + 1
|> List.create*)

Debug.loop (fun () ->
  [Metric.create (module Disorder) number_of_teams]
  |> List.iter (fun x -> print_endline "..."; Data.print ~specs x)
  ;

  schemes
  |> List.map (fun scheme -> scheme, 10000, metrics)
  |> Simulate.simulate_schemes ~specs
  |> Data.write ~specs
)
;;