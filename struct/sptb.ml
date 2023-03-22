open Util;;
open Infix;;
open Engine;;

(*THINK ABOUT POOL_TO_BRACKET SEEDING*)

module Data = Data.M (Roc);;

let symmetry_offset_helper n input=
  let rec f i n lst = match n with
    | 0 -> Some i
    | n when n < 0 -> None
    | n -> f (i + 1) (n - List.hd lst) (List.tl lst)
  in

  input
  |$> f 0 n
   |> Tuple.pair input
   |> Tuple.unsieve
  |@> Tuple.uncurry (Fun.flip Lists.top_of_list)
;;

let rec track_symmetry (scheme : Roc.t) input : int list option =
  match scheme with
  | Round_robin -> 
    input
    |@> Lists.fold (+)
    |@> Fun.flip List.init (fun _ -> 1)

  | Bracket bracket ->
    let rec f input = function
     | [] -> Some input
     | hd :: tl ->
        if hd = 0 then 
          f input tl
        else if hd < List.hd input then 
          None
        else
          f (List.tl input) ((hd - (List.hd input)) :: tl)
    in

    let rec slide = function
      | [] -> assert false
      | [n] -> [n]
      | hd :: md :: tl -> hd / 2 :: slide (md + hd / 2 :: tl)
    in
 
    input
    |$> Fun.flip f (List.rev bracket)
    |@> Tuple.pair bracket
    |@> Tuple.map_left slide
    |@> Tuple.map_left (List.filter ((<>) 0))
    |@> Tuple.map_left List.rev
    |@> Tuple.uncurry List.append

  | Pools (n, scheme) ->
    input
    |$> Options.filter (List.for_all (fun k -> k mod n = 0))
    |@> List.map (fun k -> k / n)
     |> track_symmetry scheme
    |@> List.map (fun k -> k * n)

  | Offset (n, scheme) ->
    input
    |> symmetry_offset_helper n
    |> Tuple.sieve
    |> Tuple.map_right (track_symmetry scheme)
    |> Tuple.unsieve
    |@> Tuple.uncurry List.append

  | Chain lst ->
    List.fold_left (Fun.flip track_symmetry) input lst
;;

let get_symmetric_tiers scheme n = track_symmetry scheme (Some [n])

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

let extend (specs : Data.s) (so_far : Roc.t list) : Roc.t list list =
  let k = (List.length so_far) - 1 in
  specs.number_of_teams
  |> get_symmetric_tiers (Chain so_far)
  |> symmetry_offset_helper (k)
  |> Option.value ~default:([], [])
  |> Tuple.right
  |> get_all_brackets specs.max_games
  |> List.map (fun bracket -> so_far @ [Offset (k, Bracket bracket)])
  |> List.filter  (fun lst -> Data.max_games specs.number_of_teams (Chain lst) <= specs.max_games)
;;

let build_all (specs : Data.s) : Roc.t list =
  let pool_options =
    specs.number_of_teams
    |> Math.divisors
    |> List.map (fun x -> Roc.Pools (x, Round_robin))
    |> List.filter (fun s -> Data.max_games specs.number_of_teams s <= specs.max_games)
    |> List.map (fun x -> [x])
  in
    List.map 
    (fun lst -> Roc.Chain lst)
    (List.fold_left
      (fun so_far_lst _ -> 
        List.flatten (List.map (extend specs) so_far_lst)
      )
      pool_options
      (List.init specs.number_advance Fun.id)
    )
;;