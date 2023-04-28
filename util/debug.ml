let debug = true;;

let alpha = function
  | 1 -> "E"
  | 2 -> "F"
  | 3 -> "G"
  | 4 -> "H"
  | 5 -> "I"
  | _ -> assert false
;;

let hold p x = if debug then p x else (); x;;

let mark s = hold (fun _ -> print_endline s);;

let print f = hold (fun x -> print_endline (f x));;

let rec loop f = f (); loop f;;