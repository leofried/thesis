open! Util;;

type t = float list;;

let top_out_of m n = n |> List.create |> List.map (fun i -> if i < m then 1. else 0.);;