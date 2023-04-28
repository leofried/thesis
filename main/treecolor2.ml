open Util;;
open Infix;;




let combine (f : 'a -> 'a -> Dyadic.t) ((l1, x1) : ('a * Dyadic.t) list * Dyadic.t) ((l2, x2) : ('a * Dyadic.t) list * Dyadic.t) =
  List.map (Tuple.map_right (Dyadic.mul Dyadic.half)) (l1 @ l2),
  List.map (fun (a1, s1) -> List.map (fun (a2, s2) -> Dyadic.mul (f a1 a2) (Dyadic.mul s1 s2)) l2) l1
  |> List.flatten
  |> Lists.fold Dyadic.add
  |> Dyadic.add (Dyadic.add x1 x2)
;;

let rematch (f : 'a -> 'a -> Dyadic.t) =
  Tree.map (fun x -> [x, Dyadic.one], Dyadic.zero)
  >> Tree.fold (combine f)
  >> Tuple.right
;;

let get_best_bracket (tree : int Tree.t) (f : 'a -> 'a -> Dyadic.t) (teams : 'a list list) =
  teams
  |> Lists.inner_shuffle
  |> List.map (Tuple.join (fun lst -> Tree.map (List.nth lst) tree))
  |> List.map (Tuple.map_right (rematch f))
  |> List.sort (Tuple.compare ~right:(Fun.flip Dyadic.compare))
;;
