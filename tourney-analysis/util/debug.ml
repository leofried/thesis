let hold p x = p x; x;;

let mark s = hold (fun _ -> print_endline s);;

let print f = hold (fun x -> print_endline (f x));;

let time f x =
  let t = Sys.time() in
  let fx = f x in
  print_endline ("Execution time: " ^ string_of_float (Sys.time() -. t));
  fx

let rec loop f = f (); loop f;;