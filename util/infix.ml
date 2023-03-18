let ( >> ) f g x = g (f x);;
let ( >>@ ) f g x = f (g x);;

let ( |@> ) x f = Option.map f x;;
let ( |$> ) = Option.bind;;