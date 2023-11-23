open! Util
open! Schemes
open! Metrics
open! Engine

let run () =

  let number_of_teams = 8 in

  let metric : Metric.s = {
    metric = (module Decay : Metric.S);
    specs = {
      number_of_teams;
      fidel = 0.1;
      luck = 1.;
      distr = Random.(Floored (0., gaussian));
    };
  } in


  let schemes = 
    Scheme.create (module Multibracket) [[8;0;0;0]; [2;1;0]; [4;2;0;0]] 
     :: Scheme.create (module Multibracket) [[8;0;0;0]; [4;2;0;1;0]; [2;3;0;1;0]]
     :: []
(*    (number_of_teams
    |> Multibracket.get_all_complete_simple
    |> List.map (Scheme.create (module Multibracket)))
    @
    (number_of_teams
    |> Proper.get_all
    |> List.map (Scheme.create (module Proper))) *)
  in

  let prize = Prize.top_out_of 3 8 in

  let () = Debug.loop (fun () ->
    Data.print ~metric ~prize;
    print_endline (string_of_int @@ List.length schemes);
    Simulate.simulate_schemes metric schemes 10000 |> Data.write ~metric;
  )
    
  in ()
;;