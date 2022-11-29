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

let average_float_list (lst : float list) : float =
  (List.fold_left ( +. ) 0. lst) /. (Int.to_float (List.length lst))
;;

let stats (lst : float list) : float * float * float * float=
  let mean = average_float_list lst in
  let sqers = List.map (fun x -> Float.pow (x -. mean) 2.0) lst in
  let var = average_float_list sqers in
  let stdev = Float.pow var 0.5 in
  let stder = stdev /. (Float.pow (Int.to_float (List.length lst)) 0.5) in
  let nmdev = stdev /. (List.fold_left (+.) 0. lst) in
  mean, stdev, stder, nmdev
;;

let rec find x lst =
  match lst with
  | [] -> invalid_arg "index out of bounds"
  | h :: t -> if x = h then 0 else 1 + find x t
;;


          
let pareto (lst : (string * float * float) list) : (string * float * float) list =
  let rec pareto (best : float) (lst : (string * float * float) list) : (string * float * float) list = 
    match lst with
    | [] -> []
    | (str, one, two) :: lst ->
        if compare two best < 0 then
          (str, one, two) :: pareto two lst
        else
          pareto best lst
        in
    lst
    |> List.sort (fun (_, x, _) (_, y, _) -> Float.compare x y)
    |> pareto Float.max_float
;;