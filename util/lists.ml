open Infix;;

let rec top_of_list n lst = 
  if n = 0 then [], lst else
    match lst with
    | [] -> invalid_arg "Lists.top_of_list"
    | hd :: tl -> 
      let top, bot = top_of_list (n-1) tl in
      hd :: top, bot
;;

let top_of_list_rev n lst = 
  let rec f n top lst =
    if n = 0 then top, lst else
      match lst with
      | [] -> invalid_arg "Lists.top_of_list_rev"
      | hd :: tl -> 
        f (n-1) (hd :: top) tl
  in f n [] lst
;;

let rec find x lst =
  match lst with
  | [] -> invalid_arg "Lists.find"
  | h :: t -> if x = h then 0 else 1 + find x t
;;

let fold f lst =
  match lst with
  | [] -> invalid_arg "Lists.fold"
  | [x] -> x
  | hd :: tl -> List.fold_left (fun a x -> f a x) hd tl
;;

let fold_left_i f acc =
  List.mapi (fun i x -> x, i)
  >> List.fold_left (fun acc x -> f acc x) acc
;;

let rec sum_two_lists (l1 : int list) (l2 : int list) : int list =
  match l1, l2 with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> h1 + h2 :: sum_two_lists t1 t2
  | _ -> invalid_arg "Lists.sum_two_lists"
;;

let to_string (stringify : 'a -> string) (new_line : bool) = function
  | [] -> "[]"
  | [i] -> "[" ^ (stringify i) ^ "]"
  | hd :: tl ->
    let rec f = function
    | [] -> "]"
    | hd :: tl -> (if new_line then "\n" else "") ^ ", " ^ (stringify hd) ^ f tl
    in "[ " ^ (stringify hd) ^ f tl
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

let rec verify_base_two_sum = function
  | [] -> invalid_arg "Lists.verify_base_two_sum"
  | [1] -> ()
  | [_] -> invalid_arg "Lists.verify_base_two_sum"
  | hd :: md :: tl ->
    if hd mod 2 = 1 then
      invalid_arg "Lists.verify_base_two_sum"
    else
      verify_base_two_sum (md + hd / 2 :: tl)
    ;;