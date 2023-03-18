type t = bool;;

let skip_if b f x = if b then x else f x;;
let skip_if_not b f x = if b then f x else x;;

let implies a b = Bool.not a || b;;