open! Std;;

module L = Stdlib.List;;

type 'a t = 'a list;;

let length = L.length;;
let cons = L.cons;;
let hd = L.hd;;
let tl = L.tl;;
let pop t = hd t, tl t;;
let nth = L.nth;;
let rev = L.rev;;
let create n = L.init n Fun.id;;
let rec index x = function
  | [] -> 0
  | hd :: tl -> if x = hd then 0 else 1 + index x tl
;;
let rec insert i x lst =
  match i with
  | j when j < 0 -> invalid_arg "List.insert"
  | 0 -> x :: lst
  | _ -> match lst with
    | [] -> invalid_arg "List.insert"
    | hd :: tl -> hd :: insert (i - 1) x tl
;;
let rec on_loc i f = function
  | [] -> invalid_arg "List.on_loc"
  | hd :: tl -> match i with
    | j when j < 0 -> invalid_arg "List.on_loc"
    | 0 -> f hd :: tl
    | i -> hd :: on_loc (i - 1) f tl    
;;

let append = L.append;;
let flatten = L.flatten;;
let rec top_of_list n lst = 
  if n = 0 then [], lst else
    match lst with
    | [] -> invalid_arg "Lists.top_of_list"
    | hd :: tl -> 
      let top, bot = top_of_list (n-1) tl in
      hd :: top, bot
;;

let iter = L.iter;;
let map = L.map;;
let mapi = L.mapi;;
let filter = L.filter;;
let filter_map = L.filter_map;;
let fold_left = L.fold_left;;
let fold_left_map = L.fold_left_map;;
let rec fold_downstream f x = function
  | [] -> x
  | hd :: tl -> fold_downstream f (f x hd tl) tl
;;

let assoc = L.assoc;;
let assoc_opt = L.assoc_opt;;
let split = L.split;;
let combine = L.combine;;

let rec combine_mismatched f t1 t2 =
  match t1, t2 with
  | hd1 :: tl1, hd2 :: tl2 -> f hd1 hd2 :: (combine_mismatched f tl1 tl2)
  | t, [] | [], t -> t
;;
let collapse f t1 t2 =
  fold_left (
    fun old_lst (new_id, new_data) ->
      let found, new_lst = fold_left_map
        (fun found (old_id, old_data) ->
          if new_id = old_id then
            true, (old_id, f new_data old_data)
          else 
            found, (old_id, old_data)
        )
        false old_lst
      in if found then new_lst else (new_id, new_data) :: new_lst
  ) [] (t1 @ t2)
;;

let sort = L.stable_sort;;
let sort_rev compare = sort (Fun.flip compare);;
let sort_by f compare lst =
  lst
  |> map (Pair.join_right f)
  |> sort (Pair.compare ~right:compare)
  |> map (Pair.left)
;;
let sort_by_rev f compare = sort_by f (Fun.flip compare);;
