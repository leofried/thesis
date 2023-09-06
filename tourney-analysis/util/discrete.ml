open! Std

type 'a t = ('a * float) list;;

let single x : 'a t= [x, 1.];;

let rec cross t = function
  | [] -> []
  | (hd, pct) :: tl ->
    t
    |> List.map (fun (x, pct') -> (x, hd), pct *. pct')
    |> Fun.flip List.append (cross t tl)
;;


let map f : 'a t -> 'b t = List.map (Pair.map_left f)

let collapse_verticle (t : 'a t) : 'a t = List.collapse (+.) t

let collapse_horizontal (t : 'a t t) : 'a t =
  t
  |> List.map (fun (t, p) -> List.map (fun (x, q) -> x, p *. q) t)
  |> List.flatten
;;

let expect (t : float t) : float =
  t
  |> List.map (Pair.uncurry ( *. ))
  |> List.fold_left (+.) 0.
;;