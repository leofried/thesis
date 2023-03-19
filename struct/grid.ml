open Util;;
open Infix;;

type t = Adic.t list list;;

let verify grid =
  if Bool.not @@
    Adic.equals (Adic.of_float 1.) (Lists.fold Adic.add (List.map (Lists.fold Adic.add) grid))
  then false (*print_endline "Wrong sum"; assert false*)
  else if Bool.not @@
    List.for_all (
        List.for_all (fun (x, y) ->
          Adic.to_float (Tuple.right (Adic.split_perfect x)) >= Adic.to_float (Tuple.left (Adic.split_perfect y))
        ) >>@ Lists.pair_offset
      ) grid
  then false (*print_endline "Not fair"; assert false*)
  else if Bool.not @@
    List.for_all (fun (a, b) -> Adic.is_perfect a || b mod 2 = 0) (Lists.count (List.flatten grid))
  then false (*print_endline "No games"; assert false*)
  else if Bool.not @@
    List.for_all (fun (a, b) -> Boolean.implies (List.length a = List.length b) (a = b)) (Lists.pair_offset grid)
  then false (*print_endline "Size equity inconsistent"*)
  else true
;;

let transform grid =
  let base = Lists.fold (+) (List.map (Lists.fold (+)) grid) in
  let grid = List.map (List.map (fun x -> Adic.of_float (Math.divide_int_int x base))) grid in
  if verify grid then grid else assert false
;;

let grid_to_equity : t -> Equity.t = List.map (Tuple.apply (List.length, Lists.fold Adic.add));;

let grid_to_pool_sizes : t -> int list = List.map List.length;;

let grid_to_bracket : t -> int list =
     List.flatten
  >> List.filter ((<>) (Adic.of_float 0.))
  >> List.map Adic.split_perfect
  >> List.split
  >> Tuple.uncurry List.append
  >> List.map Adic.log_base
  >> Lists.count_int
  >> List.map (Fun.flip (/) 2)
  >> List.rev
;;


let run grid =
  let rec f pool_sizes teams = match pool_sizes with
    | [] -> []
    | size :: pool_sizes ->
      let pool, teams = Lists.top_of_list size teams in
      pool :: f pool_sizes teams 
  in

  f (grid_to_pool_sizes grid)
  >> List.map Helpers.round_robin
  >> List.map2 List.combine grid
  >> Lists.unwind
  >> Lists.group_by
  >> List.map (fun (eq, lst) ->
      if Adic.is_perfect eq then 
        List.map (Tuple.pair eq) lst
      else
        let winners, losers = Helpers.round lst in
        let w_eq, l_eq = Adic.split_perfect eq in
        (List.map (Tuple.pair w_eq) (List.rev winners)) @ (List.map (Tuple.pair l_eq) losers)
    )
  >> List.flatten
  >> Lists.stable_sort_dec (Tuple.compare ~left:Adic.compare)
  >> List.split
  >> Tuple.right
  >> Helpers.bracket (grid_to_bracket grid)
;;

let get_all_grids (equity : Equity.t) ~(max_games : int) : t list =
  let f (number_of_teams, equity) =
    let base = float_of_int (Math.pow 2 (max_games - number_of_teams + 1)) in
    let eq_int = int_of_float ((Adic.to_float equity) *. base) in
    let p_int = Partition.all_parts_exact eq_int number_of_teams in
    List.map (List.map (fun x -> Adic.of_float (Math.divide_int_float x base))) p_int
  in
  List.filter verify (Lists.combos (List.map f equity))
;;

  

  



