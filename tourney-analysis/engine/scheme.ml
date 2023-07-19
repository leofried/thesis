open! Util;;
open! Std;;
open! Schemes;;

type t = string * Sexp.t [@@deriving sexp]

module type S = sig
  type t [@@deriving sexp]

  val number_of_teams : t -> int

  val run : t -> luck:float -> Team.t list -> Team.t list
end

let list : (string * (module S)) list = [
  "round_robin", (module Round_robin);
  "tof_bracket", (module Tof_bracket);
];;

let number_of_teams (kind, sexp) =
  let (module M) = List.assoc kind list in M.number_of_teams (M.t_of_sexp sexp)
;;

let run (kind, sexp) =
  let (module M) = List.assoc kind list in M.run (M.t_of_sexp sexp)
;;
