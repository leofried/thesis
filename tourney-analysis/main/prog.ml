open! Util
open! Schemes
open! Metrics
open! Engine

let run () =

  let number_of_teams = 8 in

  let metric : Metric.s = {
    metric = (module Rematches : Metric.S);
    specs = {
      number_of_teams;
      fidel = 0.;
      luck = 1.;
      distr = Random.(Floored (0., gaussian));
    };
  } in


  let schemes = 
    [
      Scheme.create (module Eight_team_group_test) WorldCup;
      Scheme.create (module Eight_team_group_test) Jumble
    ]
    
    (* @ (
    number_of_teams
    |> Proper.get_all
    |> List.map (Scheme.create (module Proper)) 
  ) *)

  in

  let prize = [1.;0.;0.;0.;0.;0.;0.;0.] in


  let () = Debug.loop (fun () ->
    Data.print ~metric ~prize;

    schemes
    |> List.map (Pair.rev 1000000)
    |> Simulate.simulate_schemes metric
    |> Data.write ~metric
    ;
  )
   in
   ()
;;