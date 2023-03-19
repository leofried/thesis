let next_part len p =
  let zeros = List.length (List.filter ((=) 0) p) in
  let ones = List.length (List.filter ((=) 1) p) in
  if ones = len then None else
  let k = len - ones - zeros - 1 in
  let p1 = List.mapi (fun i x -> if i = k then x - 1 else x) p in
  
  Some (snd @@ List.fold_left_map (fun (r, last, i) x ->
    if (i <= k) then (r, x, i + 1), x else
    if r > last then
      (r - last, last, i + 1), last
    else
      (0, r, i + 1), r
  ) (ones + 1, -1, 0) p1)
;;

let all_parts n =
  let rec f lst =
    match next_part n (List.hd lst) with
    | None -> lst
    | Some p -> f (p :: lst)
  in f [List.init n (fun i -> if i = 0 then n else 0)]
  |> List.map (List.filter ((<>) 0))
;;

let all_parts_cut n m =
  List.filter (fun x -> List.length x <= m) (all_parts n) ;;
;;

let all_parts_exact n m =
  List.filter_map (fun x ->
    let k = m - List.length x in
    if k < 0 then None
    else Some (x @ (List.init k (fun _ -> 0))))
  (all_parts n) ;;
;;