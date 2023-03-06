open Util;;
open Infix;;


type t = (int * Adic.t) list;;

let transform base = List.map (Tuple.map_right (fun x -> Adic.of_float (Math.divide_int_int x base)));;


let number_of_teams : t -> int =
  List.map Tuple.left
  >> Lists.fold (+)
;;

let validate : t -> bool = 
  List.map Tuple.right
  >> Lists.fold (Adic.add)
  >> Adic.equals (Adic.of_float 1.)
;;

let max_games : t -> int =
  List.map (fun (t, a) -> (t - 1) + (max (Adic.log_base a) (*4*) 0))
  >> Lists.fold max
;;

let equity_lineup : t -> float list =
  List.map (Tuple.map_right Adic.to_float)
  >> List.map (fun (t, e) -> List.init t (fun _ -> Math.divide_float_int e t))
  >> List.flatten
  >> List.sort Float.compare
;;

let compare x y = match List.compare Float.compare (equity_lineup x) (equity_lineup y) with
  | 0 -> List.length y - List.length x
  | z -> z
;;
