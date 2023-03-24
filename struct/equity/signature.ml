open Util;;
open Infix;;


type t = (int * Adic.t) list;;


let transform base = List.map (Tuple.map_right (fun x -> Adic.of_float (Math.divide_int_int x base)));;

let number_of_teams : t -> int =
  List.map Tuple.left
  >> Lists.fold (+)
;;

let validate : t -> bool = 
  List.map Tuple.right
  >> Lists.fold (Adic.add)
  >> Adic.equals (Adic.of_float 1.)
;;

let get_max_games : t -> int =
  List.map (fun (t, a) -> (t - 1) + (max (Adic.log_base a) (*4*) 0))
  >> Lists.fold max
;;

let team_equities : t -> float list =
  List.map (Tuple.map_right Adic.to_float)
  >> List.map (fun (t, e) -> List.init t (fun _ -> Math.divide_float_int e t))
  >> List.flatten
  >> List.sort Float.compare
;;

let compare x y = match List.compare Float.compare (team_equities y) (team_equities x) with
  | 0 -> begin match List.length x - List.length y with
    | 0 -> let f =
        List.split
        >> Tuple.left
        >> List.map float_of_int
        >> Stats.of_list
        >> Stats.stdev
      in
      compare (f x) (f y)
    | z -> z
    end
  | z -> z
;;

let make_all ~(number_of_teams : int) ~(max_games : int)
    ?(base = 32)
    ?(no_ones = true)
    ?(no_twos = true)
    ?(size_equity_consistent = true)
    ?(size_diff_max = false)
    ()
  =
  let parts = Partition.all_parts base in
  Partition.all_parts number_of_teams
  |> Boolean.skip_if_not no_ones @@ List.filter (List.for_all ((<>) 1))
  |> Boolean.skip_if_not no_twos @@ List.filter (List.for_all ((<>) 2))
  |> Boolean.skip_if_not size_diff_max @@ List.filter (fun lst -> (List.hd lst - List.nth lst (List.length lst - 1) <= 1))
  |> List.map (fun p -> List.map (List.combine p) (List.filter (fun x -> List.length x = List.length p) parts))
  |> List.flatten
  |> Boolean.skip_if_not size_equity_consistent @@ List.filter (Lists.pair_offset >> List.for_all (fun ((x, y), (w, z)) -> Boolean.implies (x = w) (y = z)))
  |> List.map (List.map (Tuple.map_right (fun x -> Adic.of_float (Math.divide_int_int x base))))
  |> List.filter (fun x -> get_max_games x <= max_games)
  |> List.sort compare
;;