open! Util
open! Schemes
open! Metrics
open! Engine

let number_of_teams = 4;;
let fidel = 1.;;
let luck = 1.01;;
let specs : Specs.t = {number_of_teams; fidel; luck};;
let metrics = Metric.empty (module Disorder) number_of_teams :: (
  number_of_teams + 1
  |> List.create
  |> List.map (Metric.empty (module Decay))
);;

let schemes = [Scheme.create (module Bracket) (Branch (Branch (Branch (Leaf 1, Leaf 2), Leaf 4), Leaf 3))];;
  
(*
Scheme.create (module Round_robin) number_of_teams :: (
  number_of_teams
  |> Bracket.get_all
  |> List.map (Scheme.create (module Bracket))
);;
*)


(*number_of_teams + 1
|> List.create*)

Debug.loop (fun () ->
  [Metric.create (module Disorder) number_of_teams]
  |> List.iter (fun x -> print_endline "..."; Data.print ~specs x)
  ;

  schemes
  |> List.map (fun scheme -> scheme, 1, metrics)
  |> Simulate.simulate_schemes ~specs
  |> Data.write ~specs

  ;

assert false
)
;;