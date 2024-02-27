open! Std

type 'a t = ('a * Rat.t) list [@@deriving sexp];;


let internal ~eq t = 
  t 
  |> List.collapse ~eq Rat.add_num 
  |> List.sort_rev (Pair.compare ~right:Rat.compare_num);;

let single x : 'a t = [x, Rat.one];;
let pair ?(eq = (=)) p x y : 'a t = [x, p; y, Rat.sub_num Rat.one p] |> internal ~eq;; 
let uniform ?(eq = (=)) lst : 'a t = lst |> List.map (Pair.rev (Rat.div_num Rat.one (Rat.num_of_int (List.length lst)))) |> internal ~eq;;


let rec cross ?(eq = (=)) t = function
  | [] -> []
  | (hd, pct) :: tl ->
    t
    |> List.map (fun (x, pct') -> (x, hd), Rat.mult_num pct pct')
    |> Fun.flip List.append (cross t tl)
  |> internal ~eq
;;

let map ?(eq = (=)) (f : 'a -> 'b) (t : 'a t) : 'b t = 
  t
  |> List.map (Pair.map_left f)
  |> internal ~eq
;;

let flatten ?(eq = (=)) (t : 'a t t) : 'a t =
  t
  |> List.map (fun (t, p) -> List.map (fun (x, q) -> x, Rat.mult_num p q) t)
  |> List.flatten
  |> internal ~eq

let expect t : Rat.t =
  t
  |> List.map (Pair.uncurry Rat.mult_num)
  |> List.fold_left Rat.add_num Rat.zero
;;

let rec equal ?(eq = (=)) a b =
  match a, b with
  | [], [] -> true
  | (a, x) :: c, (b, y) :: d ->
    eq a b && Rat.eq_num x y && equal ~eq c d
  | _ -> false
;;


let ordered compare (t : 'a t) : bool =
  let t = List.sort (Pair.compare ~left:compare) t in
  let eq = fun x y -> compare x y = 0 in
  equal ~eq t (internal ~eq t)
;;