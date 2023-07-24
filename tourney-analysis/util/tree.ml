open! Std

type 'a t =
  | Leaf of 'a
  | Branch of 'a t * 'a t
[@@deriving sexp];;


let rec count = function
  | Leaf _ -> 1
  | Branch (x, y) -> count x + count y
;;

let rec map f = function
  | Leaf x -> Leaf (f x)
  | Branch (x, y) -> Branch ((map f x), (map f y))
;;


let rec fold f = function
  | Leaf x -> x
  | Branch (x, y) -> f (fold f x) (fold f y)
;;


let rec filter f = function
  | Leaf x -> if f x then Some (Leaf x) else None
  | Branch (x, y) -> match filter f x, filter f y with
    | Some w, Some z -> Some (Branch (w, z))
    | Some w, None | None, Some w -> Some w
    | None, None -> None
;;