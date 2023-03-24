open Util;;
open Infix;;
open Engine;;

type t = Adic.t list list;;

let kind = "grid";;

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

let grid_to_equity : t -> Signature.t = List.map (Tuple.apply (List.length, Lists.fold Adic.add));;

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

let round_robin teams =
  let teams_arr = Array.of_list teams in
    
  let n = List.length teams in
  let wins_arr = Array.init n (fun _ -> Array.make n 0) in
  for i = 0 to n - 1 do
    let t1 = teams_arr.(i) in
    for j = i + 1 to n - 1 do
      let t2 = teams_arr.(j) in
        if fst @@ Team.play_game false t1 t2 = t1 then
          wins_arr.(i).(j) <- 1
        else
          wins_arr.(j).(i) <- 1
    done
  done;

  let module IMap = Map.Make(Int) in

  let score_team (games : int array array) (teams : int list) m (t1 : int) =
    let points = List.fold_left (fun x t2 -> x + games.(t1).(t2)) 0 teams in
    let new_lst =
      match IMap.find_opt points m with
      | None -> [t1]
      | Some lst -> t1 :: lst in
    IMap.add points new_lst m
  in

  let rec rank_teams (teams : Team.t list) (indicies : int list): int list =
    let scores = List.fold_left (score_team wins_arr indicies) IMap.empty indicies in
    match IMap.cardinal scores with
    | 1 -> Rand.shuffle indicies
    | _ ->
      let levels = snd @@ List.split @@ IMap.bindings @@ scores in
      List.fold_left (fun ranks level -> (rank_teams teams level) @ ranks) [] levels
  in
  
  List.map (Array.get teams_arr) (rank_teams teams (List.init (List.length teams) Fun.id))
;;

let round teams =
  let n = List.length teams / 2 in
  teams
  |> Lists.top_of_list_rev n
  |> Tuple.uncurry List.combine
  |> List.map (Tuple.uncurry (Team.play_game true))
  |> List.split
;;

let bracket bracket teams =
  Lists.verify_base_two_sum bracket;

  let rec run_reversed teams = function
    | [] -> assert false
    | [_] -> teams
    | hd :: md :: tl ->
      let players, byes = Lists.top_of_list hd teams in
      let winners, losers = round players in
      run_reversed (winners @ byes) (hd / 2 + md :: tl) @ losers
  in

  let players, dead = Lists.top_of_list (Lists.fold (+) bracket) teams in
  let order = run_reversed (List.rev players) bracket in
  order @ dead
;;


let run grid =
  let rec f pool_sizes teams = match pool_sizes with
    | [] -> []
    | size :: pool_sizes ->
      let pool, teams = Lists.top_of_list size teams in
      pool :: f pool_sizes teams 
  in

  f (grid_to_pool_sizes grid)
  >> List.map round_robin
  >> List.map2 List.combine grid
  >> Lists.unwind
  >> Lists.group_by
  >> List.map (fun (eq, lst) ->
      if Adic.is_perfect eq then 
        List.map (Tuple.pair eq) lst
      else
        let winners, losers = round lst in
        let w_eq, l_eq = Adic.split_perfect eq in
        (List.map (Tuple.pair w_eq) (List.rev winners)) @ (List.map (Tuple.pair l_eq) losers)
    )
  >> List.flatten
  >> Lists.stable_sort_dec (Tuple.compare ~left:Adic.compare)
  >> List.split
  >> Tuple.right
  >> bracket (grid_to_bracket grid)
;;

let get_all_grids (equity : Signature.t) ~(max_games : int) : t list =
  let f (number_of_teams, equity) =
    let base = float_of_int (Math.pow 2 (max_games - number_of_teams + 1)) in
    let eq_int = int_of_float ((Adic.to_float equity) *. base) in
    let p_int = Partition.all_parts_exact eq_int number_of_teams in
    List.map (List.map (fun x -> Adic.of_float (Math.divide_int_float x base))) p_int
  in
  List.filter verify (Lists.combos (List.map f equity))
;;

  

  



