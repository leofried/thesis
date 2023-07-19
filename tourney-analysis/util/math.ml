let pos_sub (x : float) (y : float) = if x < y then 0. else x -. y;;

let sqrt_int (x : int) = sqrt (Int.to_float x);;

let divide_up (x : int) (y : int) : int = if x mod y = 0 then x / y else x / y + 1;;
let divide_int_int   (x : int)   (y : int)   : float = Int.to_float x /. Int.to_float y;;
let divide_int_float (x : int)   (y : float) : float = Int.to_float x /. y;;
let divide_float_int (x : float) (y : int)   : float = x /. Int.to_float y;;

let to_pct ?(digits : int = 0) (x : float) =
  let div = 10. ** (Int.to_float digits) in
  Float.to_string (Float.round (x *. div *. 100.) /. div) ^ "%"
;;

let rec pow a = function
| 0 -> 1
| 1 -> a
| n -> 
  let b = pow a (n / 2) in
  b * b * (if n mod 2 = 0 then 1 else a)
;;

let rec gcd a b = if b = 0 then a else gcd b (a mod b);;

let divisors n =
  n
  |> List.create
  |> List.map ((+) 1)
  |> List.filter (fun x -> n mod x = 0);;

let rec log b = function
  | 1 -> 0
  | n -> 1 + if n mod b = 0 then log b (n / b) else (log b (n / b + 1))
;;