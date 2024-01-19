open! Util
open! Std
open! Schemes
open! Metrics
open! Engine

let run () =

  let number_of_teams = 10 in
  let max_games = 8 in
  let prize = [1;2;3;4] in

  let metric : Metric.s = {
    metric = (module Decay : Metric.S);
    specs = {
      number_of_teams;
      fidel = 0.;
      luck = 1.2;
      distr = Random.(Floored (0., gaussian));
      tiebreaker = WorstCase;
    };
  } in

  let schemes = 
    Pools.get_all ~respectfulness:Strongly ~triviality:Efficient ~max_games number_of_teams prize
    |> List.map (Scheme.create (module Pools))
  in

  let () = Debug.loop (fun () ->
    Data.print ~schemes ~metric ~prize ();
    print_endline (string_of_int @@ List.length schemes);
    Simulate.simulate_schemes metric schemes 1000 |> Data.write ~metric;
  )
    
  in ()
;;