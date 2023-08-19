open! Std;;

module B = Stdlib.Bool;;

type t = bool;;

let do_if b f x = if b then f x else x;;
let do_if_not b f x = if b then x else f x;;
