open! Util
open! Std
open! Schemes
open! Metrics
open! Engine

let run () =

  let number_of_teams = 12 in
  let max_games = 8 in
  let prizes = [1;3;7] in

  let metric : Metric.s = {
    metric = (module Decay : Metric.S);
    specs = {
      number_of_teams;
      fidel = 0.;
      luck = 1.;
      distr = Random.(Floored (0., gaussian));
    };
  } in

  let schemes = 
    Pools.get_all ~respectfulness:Strongly ~triviality:Efficient ~max_games number_of_teams prizes
    |> List.filter (fun f -> List.for_all (fun m -> Metrics.Faithfulness.evaluate_format f m) prizes)
    |> List.map (Scheme.create (module Pools))
  in

  let prize = Prize.convert number_of_teams prizes in

  let () = Debug.loop (fun () ->
    Data.print ~metric ~prize;
    print_endline (string_of_int @@ List.length schemes);
    Simulate.simulate_schemes metric schemes 1000 |> Data.write ~metric;
  )
    
  in ()
;;