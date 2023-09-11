open! Std

type 'a t = ('a * float) list [@@deriving sexp];;


let internal t = t |> List.collapse (+.) |> List.sort_rev (Pair.compare ~right:Float.compare);;

let single x : 'a t = [x, 1.];;
let pair p x y : 'a t = [x, p; y, 1. -. p] |> internal;; 
let uniform lst : 'a t = lst |> List.map (Pair.rev (Math.divide_int_int 1 (List.length lst))) |> internal;;


let rec cross t = function
  | [] -> []
  | (hd, pct) :: tl ->
    t
    |> List.map (fun (x, pct') -> (x, hd), pct *. pct')
    |> Fun.flip List.append (cross t tl)
  |> internal
;;

let map (f : 'a -> 'b) (t : 'a t) : 'b t = 
  t
  |> List.map (Pair.map_left f)
  |> internal
;;

let flatten (t : 'a t t) : 'a t =
  t
  |> List.map (fun (t, p) -> List.map (fun (x, q) -> x, p *. q) t)
  |> List.flatten
  |> internal

let expect (t : float t) : float =
  t
  |> List.map (Pair.uncurry ( *. ))
  |> List.fold_left (+.) 0.
;;

let ordered compare (t : 'a t) : bool =
  let t = List.sort (Pair.compare ~left:compare) t in
  t = internal t
;;
