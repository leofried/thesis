open Util;;

type t = int list;;
           
let is_empty (lst : t) = lst = [] (*&& x = None;;*)
         
let pop : t -> int * t = Tuple.apply (List.hd, List.tl) (*function
  | ([], None) -> assert false
  | ([], Some x) -> x, ([], Some x)
  | (hd :: tl, x) -> hd, (tl, x) *)
;;

let rec compatable (t1 : t) (t2 : t) =
  match t1, t2 with
  | _, [] -> true
  | [], _ -> false
  | hd1 :: tl1, hd2 :: tl2 ->
    if hd1 < hd2 then compatable tl1 (hd2 - hd1 :: tl2)
    else if hd1 = hd2 then compatable tl1 tl2
    else false
;;

let compose (t1 : t) (t2 : t) =
  let rec f lst = function
    | 0 -> lst
    | x -> f (List.tl lst) (x - List.hd lst)
  in
  t2 @ f t1 (Lists.fold (+) t2)
;;