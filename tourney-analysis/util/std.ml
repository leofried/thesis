include Sexplib.Std;;

let ( >> ) f g x = g (f x);;
(*
let (|?>) x f = Option.map f x;;
*)