type t = int list;;
 
let convert n t =
  let f n m = n |> List.create |> List.map (fun i -> if i < m then 1. else 0.) in
  t
  |> List.map (f n)
  |> List.fold_left (fun a b -> List.combine a b |> List.map (fun (x, y) -> x +. y)) (f n 0)
;;