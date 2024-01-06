open! Util;;

type t = float list;;

let top_out_of m n = n |> List.create |> List.map (fun i -> if i < m then 1. else 0.);;

let convert n prizes : float list =
  prizes
  |> List.map (Fun.flip top_out_of n)
  |> List.fold_left (fun a b -> List.combine a b |> List.map (fun (x, y) -> x +. y)) (top_out_of 0 n)