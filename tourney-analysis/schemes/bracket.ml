open! Util
open! Std

type t = int Tree.t;;

let rec sexp_of_t : t -> Sexp.t = function
  | Tree.Leaf x -> sexp_of_int x
  | Branch (t1, t2) -> List [sexp_of_t t1; Atom "v"; sexp_of_t t2]
;;

let rec t_of_sexp sexp = match (sexp : Sexp.t) with
  | Atom s -> Tree.Leaf (int_of_string s)
  | List [s1; Atom "v"; s2] -> Tree.Branch (t_of_sexp s1, t_of_sexp s2) 
  | List _ -> raise (Sexplib.Conv_error.unexpected_stag "bracket.ml" sexp)
;;

let kind = "bracket";;

let number_of_teams = Tree.count;;

let combine_losers lst1 = List.combine_mismatched (fun lst1 lst2 -> List.append lst1 lst2 |> List.shuffle) lst1;;

let run bracket game teams =
  bracket
  |> Tree.map_und (fun n -> List.nth teams (n - 1), [])
  |> Tree.fold (fun (hd1, tl1) (hd2, tl2) ->
    let w, l = game.Game.play hd1 hd2 in
    w, [l] :: combine_losers tl1 tl2
  )
  |> Pair.map_left (fun x -> [x])
  |> Pair.uncurry List.cons
;;


let rec get_all_from_n = function
  | 1 -> [Tree.Leaf 1]
  | n ->
    let rec add_all t =
      Tree.Branch (t, Leaf n) :: match t with
      | Leaf _ -> []
      | Branch (t1, t2) ->
        List.map (fun t1 -> Tree.Branch (t1, t2)) (add_all t1) @
        List.map (fun t2 -> Tree.Branch (t1, t2)) (add_all t2)
    in
    (n - 1)
    |> get_all_from_n
    |> List.map add_all
    |> List.flatten
;;


let get_all_from_shape (shape : unit Tree.t) : t list =
  shape
  |> Tree.count
  |> get_all_from_n
  |> List.filter (Tree.same_shape true shape)
;;



