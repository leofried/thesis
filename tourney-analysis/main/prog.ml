open! Util
open! Schemes
open! Struct
open! Metrics
open! Engine

let run () =

  let s = Scheme.create (module Proper) [2;3;2;0] in
  let number_of_teams = Scheme.number_of_teams s in

  let metric : Metric.s = {
    metric = (module Decay : Metric.S);
    specs = {
      number_of_teams;
      fidel = 0.5;
      luck = 3.;
      distr = Random.gaussian;
    };
  } in

  let schemes = [s; Scheme.create (module Proper) [2;3;2;0;0]] in
  (*(
  Eight_team_double_elim.[NoGame; Losers; WinnersOne; WinnersTwo]
    |> Eight_team_double_elim.get_all
    |> List.map (Scheme.create (module Eight_team_double_elim))
  )
  @

  Scheme.create (module Round_robin) number_of_teams :: 
  (
    Branch (
      Branch (
        Branch (Leaf (), Leaf ()),
        Branch (Leaf (), Leaf ())
      ),
      Branch (
        Branch (Leaf (), Leaf ()),
        Branch (Leaf (), Leaf ())
      )
    )
    |> Bracket.get_all_from_shape

        
  (*  number_of_teams
    |> Bracket.get_all_from_n 
  *)  |> List.map (Scheme.create (module Bracket))
  ) in*)
  (*
  [
    Scheme.t_of_sexp @@ Sexp.of_string
    "(bracket(((1 v 8)v(3 v 7))v((2 v 6)v(4 v 5))))"
    (*"((((1 v 16)v(8 v 9))v((4 v 13)v(5 v 12)))v(((2 v 15)v(7 v 10))v((3 v 14)v(6 v 11))))"*)
  ]
   in*)
  (*
  let prize = [
    1.; 0.5; 1./.3.; 0.25;
    (0.2+.1./.6.)/.2.; (0.2+.1./.6.)/.2.;
    (0.125+.1./.7.)/.2.; (0.125+.1./.7.)/.2.;
  ] in*)

  let prize = [1.;0.;0.;0.;0.;0.;0.] in


  let () = Debug.loop (fun () ->
    Data.print ~metric ~prize;

    schemes
    |> List.map (Pair.rev 1000)
    |> Simulate.simulate_schemes metric
    |> Data.write ~metric
    ;
  )
   in
   ()
;;