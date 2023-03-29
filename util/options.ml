let filter f x = if f x then Some x else None;;

let to_string f = function
  | None -> "None"
  | Some x -> f x
;;