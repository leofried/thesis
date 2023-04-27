open Util;;
open Infix;;


(*type t = int;;*)
type seed = int;;

let look (target : seed) : (seed * 'b option) Tree.t -> 'b list = 
  let rec f = function
    | Tree.Leaf (x, None) -> ([], x = target)
    | Leaf (x, Some c) -> ([x, c], false)
    | Branch (x, y) ->
      match f x, f y with
      | (l1, false), (l2, false) -> (l1 @ l2), false
      | (f, false), (t, true) | (t, true), (f, false) -> t @ (List.sort (Tuple.compare ~left:Int.compare) f), true
      | (_, true), (_, true) -> assert false
  in
  f
  >> Tuple.left
  >> List.split
  >> Tuple.right
;;

let select (options : ('a * 'b) list) (nearby : 'b list) : 'a =
  options
  |> List.map (fun (id, c) -> id, Lists.find c nearby)
  |> List.stable_sort (Tuple.compare ~right:(Fun.flip Int.compare))
  |> List.hd
  |> Tuple.left
;;

let assign (target : seed) (color : 'b) = Tree.map (fun (x, o) -> x, match o with
  | None -> if x = target then Some color else None
  | Some d -> Some d
  )
;;

let rec loop (tree : (seed * 'b option) Tree.t) : ('a * 'b) list -> (seed * 'b option) Tree.t * 'a list = function
  | [] -> tree, []
  | options ->
    let target = 
      tree
      |> Tree.filter (Tuple.right >> Option.is_none)
      |> Option.get
      |> Tree.map (Tuple.left)
      |> Tree.fold min
    in

    let selected =
      tree
      |> look target
      |> select options
    in

    loop (assign target (List.assoc selected options) tree) (List.remove_assoc selected options)
    |> Tuple.map_right (List.cons selected)
;;

let color (tree : seed Tree.t) : ('a * 'b) list list -> 'a list =
  List.fold_left_map loop (Tree.map (fun x -> x, None) tree)
  >> Tuple.right
  >> List.flatten
;;



