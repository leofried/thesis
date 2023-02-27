let left = fst;;

let right = snd;;

let pair a b = (a, b);;

let seive = function
  | None, _ -> None
  | _, None -> None
  | Some x, Some y -> Some (x, y)
;;

let map_left f (a, b) = f a, b;;

let map_right f (a, b) = a, f b;;

let commute (a, b) = (b, a);;

let associate_left (a, (b, c)) = ((a, b), c);;

let associate_right ((a, b), c) = (a, (b, c));;

let curry f a b = f (a, b);;

let uncurry f (a, b) = f a b;;

