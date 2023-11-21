open! Util
open! Std
open! Schemes

module type S = sig
  type t [@@deriving sexp]

  val kind : string

  val generate : Specs.t -> Scheme.t list -> t list

  val empty : Specs.t -> t

  val combine : t -> t -> t

  val score : t -> Prize.t -> float * float * int
end

type s = {
  metric: (module S);
  specs: Specs.t;
}
type t = Sexp.t [@@deriving sexp];;

let kind (s : s) = let (module M) = s.metric in M.kind;;

let generate (s : s) (schemes : Scheme.t list) = let (module M) = s.metric in
  List.map M.sexp_of_t (M.generate s.specs schemes)
;;

let empty (s : s) : t = let (module M) = s.metric in
  M.sexp_of_t (M.empty s.specs)
;;

let combine (s : s) t1 t2 : t = let (module M) = s.metric in
  M.sexp_of_t (M.combine (M.t_of_sexp t1) (M.t_of_sexp t2))
;;

let score (s : s) prize t = let (module M) = s.metric in
  M.score (M.t_of_sexp t) prize
;;