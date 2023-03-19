let left = fst;;

let right = snd;;

let pair a b = (a, b);;

let swap (a, b) = (b, a);;

let unsieve = function
  | None, _ -> None
  | _, None -> None
  | Some x, Some y -> Some (x, y)
;;

let sieve = function
  | None -> None, None
  | Some (x, y) -> Some x, Some y
;;

let map_left f (a, b) = f a, b;;

let map_right f (a, b) = a, f b;;

let associate_left (a, (b, c)) = ((a, b), c);;

let associate_right ((a, b), c) = (a, (b, c));;

let curry f a b = f (a, b);;

let uncurry f (a, b) = f a b;;

let apply (f, g) x = f x, g x;;

let compare ?(left = fun _ _ -> 0) ?(right = fun _ _ -> 0) (a, b) (c, d) =
  match left a c with
  | 0 -> right b d
  | z -> z
;;

