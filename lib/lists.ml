let is_not_empty lst = lst <> [];;

let rec top_of_list lst n = 
  if n = 0 then [], lst else
    match lst with
    | [] -> invalid_arg "index out of bounds"
    | hd :: tl -> 
      let top, bot = top_of_list tl (n-1) in
      hd :: top, bot
;;

let rec sum_two_lists (l1 : int list) (l2 : int list) : int list =
  match l1, l2 with
  | _, [] -> l1
  | [], _ -> l2
  | h1 :: t1, h2 :: t2 -> h1 + h2 :: sum_two_lists t1 t2
;;

let to_string (stringify : 'a -> string) = function
  | [] -> "[]"
  | [i] -> "[" ^ (stringify i) ^ "]"
  | hd :: tl ->
    let rec f = function
    | [] -> "]"
    | hd :: tl -> ", " ^ (stringify hd) ^ f tl
    in "[" ^ (stringify hd) ^ f tl
;;

let average (lst : float list) : float =
  (List.fold_left ( +. ) 0. lst) /. (Int.to_float (List.length lst))
;;

let stats (lst : float list) : float * float * float =
  let mean = average lst in
  let sqers = List.map (fun x -> Float.pow (x -. mean) 2.0) lst in
  let var = average sqers in
  let stdev = Float.pow var 0.5 in
  let stder = stdev /. (Float.pow (Int.to_float (List.length lst)) 0.5) in
  mean, stdev, stder
;;