open! Util
open! Std

type t = string * Sexp.t [@@deriving sexp]

module type S = sig
  type t [@@deriving sexp]

  val kind : string

  val number_of_teams : t -> int

  val run : t -> 'a Game.t -> 'a list -> 'a list list
end

let list : (module S) list = [
  (module Round_robin);
  (module Bracket);
  (module Proper);
  (module Multibracket);
  (module Pools);
  (module Mlq);
];;

let get kind =
  list
  |> List.map (Pair.join_left (fun (module M : S) -> M.kind))
  |> List.assoc kind
;;

let create (type a) (module M : S with type t = a) (arg : a) : t=
  M.kind, M.sexp_of_t arg
;;

let number_of_teams (kind, sexp) =
  let (module M) = get kind in M.number_of_teams (M.t_of_sexp sexp)
;;

let run (kind, sexp) play teams =
  let (module M) = get kind in List.flatten (M.run (M.t_of_sexp sexp) play teams)
;;