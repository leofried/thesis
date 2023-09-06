type 'a t = 'a list;;

let permutations = 
  let rec f x = function
    | [] -> [[x]]
    | hd :: tl as lst ->
      (x :: lst) :: (List.map (fun l -> hd :: l) (f x tl))
  in let rec g = function
  | [] -> [[]]
  | hd :: tl ->
    let perms = g tl in
    List.fold_left (fun acc p -> acc @ f hd p) [] perms
  in g
;;

let combinations t =
  let rec combine acc current_lists =
    match current_lists with
    | [] -> [List.rev acc]
    | hd_list :: tl_lists ->
      List.fold_left
        (fun acc' hd_elem -> acc' @ combine (hd_elem :: acc) tl_lists)
        [] hd_list
  in
  combine [] t
;;

let partitions n =
  let rec upped_partitions i n =
    if i = n then [[[n]]] else
    upped_partitions (i + 1) n
    |> List.map (fun lst -> [[i] :: lst; (i :: List.hd lst) :: List.tl lst])
    |> List.flatten
  in
  upped_partitions 1 n
;; 
    
