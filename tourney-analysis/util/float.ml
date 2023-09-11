open! Std;;

module F = Stdlib.Float;;

type t = float [@@deriving sexp]

let delta = 1e-10;;

let compare t1 t2 = if abs_float (t1 -. t2) < delta then 0 else F.compare t1 t2;;

let to_int = F.to_int;;
let to_string = F.to_string;;
let round = F.round;;