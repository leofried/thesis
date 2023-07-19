module L = Stdlib.List;;

type 'a t = 'a list;;

let length = L.length;;
let cons = L.cons;;
let hd = L.hd;;
let tl = L.tl;;
let nth = L.nth;;
let rev = L.rev;;
let create n = L.init n Fun.id;;
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
let filter = L.filter;;
let filter_map = L.filter_map;;
let fold_left = L.fold_left;;
let fold_right = L.fold_right;;
let fold_left_map = L.fold_left_map;;


let assoc = L.assoc;;
let assoc_opt = L.assoc_opt;;
let split = L.split;;
let combine = L.combine;;
let collapse f t1 t2=
  fold_left (
    fun old_lst (new_id, new_data) ->
      let found, new_lst = fold_left_map
        (fun found (old_id, old_data) ->
          if new_id = old_id then
            true, (old_id, f old_id new_data old_data)
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
  |> map (Pair.join f)
  |> sort (Pair.compare ~right:compare)
  |> map (Pair.left)
;;
let sort_by_rev f compare = sort_by f (Fun.flip compare);;
