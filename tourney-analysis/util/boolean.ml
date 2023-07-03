type t = bool;;

let do_if b f x = if b then f x else x;;
let skip_if b f x = if b then x else f x;;

let implies a b = Bool.not a || b;;