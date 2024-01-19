type s = int list;;
type t = float list;;
 
let convert n (prizes : s) : t =
  let f n m = n |> List.create |> List.map (fun i -> if i < m then 1. else 0.) in
  prizes
  |> List.map (f n)
  |> List.fold_left (fun a b -> List.combine a b |> List.map (fun (x, y) -> x +. y)) (f n 0)
;;