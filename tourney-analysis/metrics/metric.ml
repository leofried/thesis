open! Util
open! Std
open! Struct
open! Schemes

module type S = sig
  type t [@@deriving sexp]

  val kind : string

  val empty : Specs.t -> t

  val fold : t -> Specs.t -> Scheme.t -> t

  val combine : t -> t -> t

  val score : t -> float * float * int
end

type s = {
  metric: (module S);
  specs: Specs.t;
}
type t = Sexp.t [@@deriving sexp];;

let kind (s : s) = let (module M) = s.metric in M.kind;;

let create (s : s): t = let (module M) = s.metric in
  M.sexp_of_t (M.empty s.specs)

let fold (s : s) t scheme : t = let (module M) = s.metric in
  (M.sexp_of_t (M.fold (M.t_of_sexp t) s.specs scheme))
;;

let combine (s : s) t1 t2 : t = let (module M) = s.metric in
  M.sexp_of_t (M.combine (M.t_of_sexp t1) (M.t_of_sexp t2))
;;

let score (s : s) t = let (module M) = s.metric in
  M.score (M.t_of_sexp t)
;;