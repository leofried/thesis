let inc (r : int ref) : unit = r := !r + 1;;

let inc_array (arr : float array) (i : int) : unit = arr.(i) <- arr.(i) +. 1.;;

let divide (x : int) (y : int) : float = Int.to_float x /. Int.to_float y;;

let rec pow a = function
| 0 -> 1
| 1 -> a
| n -> 
  let b = pow a (n / 2) in
  b * b * (if n mod 2 = 0 then 1 else a)
;;

let to_pct ?(digits : int = 0) (x : float) =
  let div = Int.to_float (pow 10 (digits)) in
  Float.to_string (Float.round (x *. div *. 100.) /. div) ^ "%"
;;