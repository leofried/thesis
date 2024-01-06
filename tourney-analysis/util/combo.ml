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

let compositions n =
  let rec upped_compositions i n =
    if i = n then [[[n]]] else
    upped_compositions (i + 1) n
    |> List.map (fun lst -> [[i] :: lst; (i :: List.hd lst) :: List.tl lst])
    |> List.flatten
  in
  upped_compositions 1 n
;; 

let rec partitions ~n ?m ?(l = -1) () = 
  if n = 0 then 
    if l <= 0 then [[]] else [l |> List.create |> List.map (Fun.const 0)]
  else if n < 0 || l = 0 then []
  else
    m
    |> Option.fold n Fun.id
    |> List.create
    |> List.map ((+) 1)
    |> List.map (fun x -> List.map (List.cons x) (partitions ~n:(n - x) ~m:x ~l:(l - 1) ()))
    |> List.flatten 
;;
    
let ssts n =
  let rec upped_ssts i n =
    if i = n then [[n]] else
    upped_ssts (i + 1) n
    |> List.map (fun lst -> (List.hd lst - i + 1) |> List.create |> List.map ((+) i) |> List.map (Fun.flip List.cons lst))
    |> List.flatten
  in
  upped_ssts 0 (n-1)