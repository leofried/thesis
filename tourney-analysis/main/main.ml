open! Util
open! Schemes
open! Struct
open! Metrics
open! Engine

let number_of_teams = 3;;

let metric : Metric.s = {
  metric = (module Decay : Metric.S);
  specs = {
    number_of_teams;
    fidel = 1.;
    luck = 1.;
  };
};;

let schemes = Scheme.create (module Round_robin) number_of_teams :: (
  number_of_teams
  |> Bracket.get_all
  |> List.map (Scheme.create (module Bracket))
);;

Debug.loop (fun () ->
  print_endline "---";
  Data.print ~metric;

  schemes
  |> List.map (Pair.rev 100000)
  |> Simulate.simulate_schemes ~metric
  |> Data.write ~metric
)
;;