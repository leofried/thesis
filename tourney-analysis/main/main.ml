open! Util
open! Schemes
open! Struct
open! Metrics
open! Engine

let number_of_teams = 8;;

let metric : Metric.s = {
  metric = (module Disorder : Metric.S);
  specs = {
    number_of_teams;
    fidel = 1.;
    luck = 1.;
    distr = Floored (0., Random.gaussian);
  };
};;

let schemes = (*Scheme.create (module Round_robin) number_of_teams :: 
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
)*)

[
  Scheme.t_of_sexp @@ Sexp.of_string
  "(bracket(((1 v 8)v(3 v 7))v((2 v 6)v(4 v 5))))"
  (*"((((1 v 16)v(8 v 9))v((4 v 13)v(5 v 12)))v(((2 v 15)v(7 v 10))v((3 v 14)v(6 v 11))))"*)
]
;;


let prize = [
  1.; 0.; 0.; 0.; 0.; 0.; 0.; 0.;
(*  0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; *)
];;


Debug.loop (fun () ->
  print_endline "---";
  Data.print ~metric ~prize;
  print_endline (string_of_int (List.length schemes));

  schemes
  |> List.map (Pair.rev 100000)
  |> Simulate.simulate_schemes ~metric
  |> Data.write ~metric
  ;
)
;;