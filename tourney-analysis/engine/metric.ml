open! Util;;
open! Std;;
open! Metrics;;

type t = string * Sexp.t [@@deriving sexp];;
type u = Sexp.t * int [@@deriving sexp];;
type v = t * u [@@deriving sexp];;

module type S = sig
  type t [@@deriving sexp]
  type u [@@deriving sexp]

  val kind : string

  val empty : t -> u

  val fold : t -> u -> teams:Team.t list -> results:Team.t list -> u

  val combine : t -> u -> u -> u

  val score : t -> u -> float * float
end

let create (type a) (module M : S with type t = a) (arg : a) : t =
  M.kind, M.sexp_of_t arg
;;

let empty (type a) (module M : S with type t = a) (arg : a) : v =
  (M.kind, M.sexp_of_t arg),
  (M.sexp_of_u (M.empty arg), 0)
;;

let list : (module S) list = [
  (module Decay);
  (module Disorder);
];;

let get kind =
  list
  |> List.map (Pair.join_left (fun (module M : S) -> M.kind))
  |> List.assoc kind
;;

let fold ~teams ~results ((kind, sexp1) as t, (sexp2, n) : v) : v =
  let (module M) = get kind in t, (M.sexp_of_u (M.fold (M.t_of_sexp sexp1) (M.u_of_sexp sexp2) ~teams ~results), n+1)
;;

let combine (kind, sexp1 : t) ((sexp2, n) : u) (sexp3, m : u) : u = 
  let (module M) = get kind in M.sexp_of_u (M.combine (M.t_of_sexp sexp1) (M.u_of_sexp sexp2) (M.u_of_sexp sexp3)), n + m
;;

let score ((kind, sexp1), (sexp2, n) : v) =
  let (module M) = get kind in let a, b = M.score (M.t_of_sexp sexp1) (M.u_of_sexp sexp2) in a, b, n