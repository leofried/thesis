open! Util
open! Std
open! Schemes

type t = {
  number_of_teams : int;
  luck : float;
  fidel : float;
  distr : Random.f;
  tiebreaker : Game.tiebreaker;
} [@@deriving sexp];;

(* let default = {
  number_of_teams = 0;
  luck = 1.;
  fidel = 0.;
  distr = Random.gaussian;
  tiebreaker = Game.Random;
} *)