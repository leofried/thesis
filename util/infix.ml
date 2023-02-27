let ( >> ) f g x = g (f x);;

let ( |@> ) x f = Option.map f x;;
let ( |$> ) = Option.bind;;