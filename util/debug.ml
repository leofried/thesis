let debug = false;;

let hold p x = if debug then p x else (); x;;

let mark s = hold (fun _ -> print_endline s);;

let print f = hold (fun x -> print_endline (f x));;

let rec loop f = f (); loop f;;