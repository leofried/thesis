type ('a, 'b) t = 'a * 'b [@@deriving sexp];;

let left = fst;;

let right = snd;;

let pair a b = (a, b);;

let rev a b = (b, a);;

let swap (a, b) = (b, a);;

let map f (a, b) = f a, f b;;

let map_left f (a, b) = f a, b;;

let map_right f (a, b) = a, f b;;


let apply (f, g) x = f x, g x;;

let associate_left (a, (b, c)) = ((a, b), c);;

let associate_right ((a, b), c) = (a, (b, c));;

let curry f a b = f (a, b);;

let uncurry f (a, b) = f a b;;

let join_left f x = f x, x;;

let join_right f x = x, f x;;

let compare ?(left = fun _ _ -> 0) ?(right = fun _ _ -> 0) (a, b) (c, d) =
  match left a c with
  | 0 -> right b d
  | z -> z
;;

let to_string left right (a, b) = "(" ^ left a ^ ", " ^ right b ^ ")";;

