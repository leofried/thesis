include Lwt.Infix;;

let ( >> ) g f x = f (g x);;

