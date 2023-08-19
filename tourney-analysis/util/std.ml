include Sexplib.Std;;

let ( >> ) f g x = g (f x);;

let (|?>) x b f = if b then f x else x;;