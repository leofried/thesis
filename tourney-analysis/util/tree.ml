open! Std

type 'a t =
  | Leaf of 'a
  | Branch of 'a t * 'a t
[@@deriving sexp];;


let rec count = function
  | Leaf _ -> 1
  | Branch (x, y) -> count x + count y
;;

let rec depth = function
  | Leaf _ -> 0
  | Branch (x, y) -> 1 + Int.max (depth x) (depth y)
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

let rec same_shape (reflective : bool) t1 t2 = match t1, t2 with
  | Leaf _, Leaf _ -> true
  | Leaf _, Branch _ | Branch _, Leaf _ -> false
  | Branch (t11, t12), Branch (t21, t22) ->
    (
      same_shape reflective t11 t21 &&
      same_shape reflective t12 t22
    ) ||
    (reflective && 
      (
        same_shape reflective t11 t22 &&
        same_shape reflective t12 t21
      )
    )
;;