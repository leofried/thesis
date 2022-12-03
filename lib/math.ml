let inc_ref (r : int ref) : unit = r := !r + 1;;

let inc_array (arr : int array) (i : int) : unit = arr.(i) <- arr.(i) + 1;;

let sqrt_int (x : int) = sqrt (Int.to_float x);;

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