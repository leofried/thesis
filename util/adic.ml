type t = int * int;;


let rec of_float f : t =
  if ceil f = floor f then (int_of_float f, 0)
  else if f > 1_000_000. then invalid_arg "Adic.of_float"
  else let (x, y) = of_float (2. *. f) in (x, y + 1)
;;

let to_float (x, y) = Math.divide_int_int x (Math.pow 2 y)

let rec reduce (x, y) =
  if y > 0 && x mod 2 = 0 then
    reduce (x / 2, y - 1)
  else
    (x, y)
;;

let compare x y = Float.compare (to_float x) (to_float y)

let equals x y = reduce x = reduce y;;

let rec expand (x, y) z =
  if y = z then (x, y)
  else expand (2 * x, y + 1) z
;;

let add (x1, y1) (x2, y2) =
  let y3 = max y1 y2 in
  let (x3, _) = expand (x1, y1) y3 in
  let (x4, _) = expand (x2, y2) y3 in
  reduce (x3 + x4, y3)
;;

let log_base x = 
  Tuple.right (reduce x)
;;

let split_perfect (x, y) =
  if x = 0 then ((0, 1), (0, 1)) else
  let b = Math.next_pow 2 x in
  let a = 2 * x - b in
  if Math.next_pow 2 a = a then (reduce (b, y), reduce (a, y))
  else invalid_arg "Adic.split_perfect"
;;