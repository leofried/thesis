type 'a t =
  | Leaf of 'a
  | Branch of 'a t * 'a t
;;

let rec to_string (stringy : 'a -> string) (tree : 'a t) : string =
  match tree with
  | Leaf x -> stringy x
  | Branch (t1, t2) -> "(" ^ to_string stringy t1 ^ " v " ^ to_string stringy t2 ^ ")"
;;    

