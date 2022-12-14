let rec top_of_list lst n = 
  if n = 0 then [], lst else
    match lst with
    | [] -> invalid_arg "index out of bounds"
    | hd :: tl -> 
      let top, bot = top_of_list tl (n-1) in
      hd :: top, bot
;;

let rec find x lst =
  match lst with
  | [] -> invalid_arg "index out of bounds"
  | h :: t -> if x = h then 0 else 1 + find x t
;;

let rec sum_two_lists (l1 : int list) (l2 : int list) : int list =
  match l1, l2 with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> h1 + h2 :: sum_two_lists t1 t2
  | _ -> System.error ()
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
       
let pareto (lst : ('a * float * float) list) : ('a * float * float) list =
  let rec pareto (best : float) (lst : ('a * float * float) list) : ('a * float * float) list = 
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