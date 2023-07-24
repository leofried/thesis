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
  | List _ -> print_endline (Sexp.to_string sexp); assert false
;;



let kind = "bracket";;

let number_of_teams = Tree.count;;

let run bracket ~luck teams =
  let rec play = function
    | Tree.Leaf t -> t, []
    | Branch (t1, t2) ->
      let hd1, tl1 = play t1 in
      let hd2, tl2 = play t2 in
      let w, l = Team.play_game ~luck ~is_bracket:true hd1 hd2 in
      w, [l] :: List.combine_mismatched List.append tl1 tl2
  in
  bracket
  |> Tree.map (fun n -> List.nth teams (n - 1))
  |> play
  |> Pair.map_right List.flatten
  |> Pair.uncurry List.cons
;;

let rec get_all = function
  | 1 -> [Tree.Leaf 1]
  | n ->
    let rec add_all t =
      Tree.Branch (t, Leaf n) :: match t with
      | Leaf _ -> []
      | Branch (t1, t2) ->
        List.map (fun t1 -> Tree.Branch (t1, t2)) (add_all t1) @
        List.map (fun t2 -> Tree.Branch (t1, t2)) (add_all t2)
    in
    get_all (n - 1)
    |> List.map add_all
    |> List.flatten
;;