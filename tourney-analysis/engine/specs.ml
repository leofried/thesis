open! Util
open! Std

type t = {
  number_of_teams : int;
  luck : float;
  fidel : float;
  distr : Random.f;
} [@@deriving sexp];;

let default = {
  number_of_teams = 0;
  luck = 1.;
  fidel = 0.;
  distr = Random.gaussian;
}