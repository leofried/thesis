open Util;;

let get_all_brackets (max_games : int) (tiers : int list) : int list list =
  let rec f (max_games : int) (target_sum : int) (tiers : int list) (sq : bool) : int list list =
    if target_sum = 0 then [[]] else
    if max_games < 0 then [] else
      match tiers with
      | [] -> []
      | hd :: tl -> 
          let zs = if sq then [] else List.map (fun lst -> 0 :: lst) (f (max_games - 1) (target_sum * 2) (tiers) false) in
          let hds = List.map (fun lst -> hd :: lst) (f (max_games - 1) ((target_sum - hd) * 2) (tl) false) in
          let lst =
            match tl with
            | [] -> []
            | md :: tl -> f max_games target_sum ((hd + md) :: tl) true
          in List.concat [zs; hds; lst]
  in List.map List.rev (f max_games 1 tiers false)
;;


let extend ~(number_of_teams : int) ~(max_games : int) (so_far : Scheme.t list) : Scheme.t list list =
  let k = (List.length so_far) - 1 in
  number_of_teams
  |> Scheme.get_symmetric_tiers (Chain so_far)
  |> Scheme.symmetry_offset_helper (k)
  |> Option.value ~default:([], [])
  |> Tuple.right
  |> get_all_brackets max_games
  |> List.map (fun bracket -> so_far @ [Offset (k, Bracket bracket)])
  |> List.filter  (fun lst -> Scheme.max_games (Chain lst) number_of_teams <= max_games)
;;


let build_all ~(number_of_teams) ~(max_games) ~(number_advance) : Scheme.t list =
  let pool_options =
    number_of_teams
    |> Math.divisors
    |> List.map (fun x -> Scheme.Pools (x, Round_robin))
    |> List.filter (fun s -> Scheme.max_games s number_of_teams <= max_games)
    |> List.map (fun x -> [x])
  in
    List.map 
    (fun lst -> Scheme.Chain lst)
    (List.fold_left
      (fun so_far_lst _ -> 
        List.flatten (List.map (extend ~number_of_teams ~max_games) so_far_lst)
      )
      pool_options
      (List.init number_advance Fun.id)
    )
;;