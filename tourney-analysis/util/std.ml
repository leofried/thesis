include Sexplib.Std;;

let ( >> ) f g x = g (f x);;


(* (* 
let (|?>) x f = Option.map f x;;
let (|??>) x f = Option.bind f x;; *)
let (let* )  x f = Option.bind f x;; *)