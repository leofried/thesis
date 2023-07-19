open! Util;;
open! Std;;
open! Metrics;;

type t = string * Sexp.t [@@deriving sexp];;
type u = Sexp.t * int [@@deriving sexp];;
type v = t * u [@@deriving sexp];;

module type S = sig
  type t [@@deriving sexp]
  type u [@@deriving sexp]

  val empty : t -> u

  val fold : t -> u -> teams:Team.t list -> results:Team.t list -> u

  val combine : t -> u -> u -> u

  val score : t -> u -> float * float
end

let list : (string * (module S)) list = [
  "decay", (module Decay);
];;

let empty (kind, sexp as t : t) : v =
  let (module M) = List.assoc kind list in t, (M.sexp_of_u (M.empty (M.t_of_sexp sexp)), 0)
;;

let fold ~teams ~results ((kind, sexp1) as t, (sexp2, n) : v) : v =
  let (module M) = List.assoc kind list in t, (M.sexp_of_u (M.fold (M.t_of_sexp sexp1) (M.u_of_sexp sexp2) ~teams ~results), n+1)
;;

let combine (kind, sexp1 : t) ((sexp2, n) : u) (sexp3, m : u) : u = 
  let (module M) = List.assoc kind list in M.sexp_of_u (M.combine (M.t_of_sexp sexp1) (M.u_of_sexp sexp2) (M.u_of_sexp sexp3)), n + m
;;

let score ((kind, sexp1), (sexp2, n) : v) =
  let (module M) = List.assoc kind list in let a, b = M.score (M.t_of_sexp sexp1) (M.u_of_sexp sexp2) in a, b, n