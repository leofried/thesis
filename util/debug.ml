let mark s x = print_endline s; x;;
let hold p x = p x; x;;

let rec loop f = f (); loop f;;