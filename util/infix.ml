let ( >> ) f g x = g (f x);;

let ( |@> ) x f = Option.map f x;;
let ( |$> ) = Option.bind;;

let ( |~> ) x f = match x with
| None -> None
| Some x as y -> if f x then y else None
;;