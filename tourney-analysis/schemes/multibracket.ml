open! Util;;
open! Std;;

type t = Proper.t list [@@deriving sexp];;

let kind = "multibracket";;

let number_of_teams t =
  t
  |> List.fold_left (fun (w, l) b -> let new_w = Proper.number_of_winners b in (w + new_w), (max l (Proper.number_of_teams b) - new_w)) (0, 0)
  |> Pair.uncurry (+)
;;

let rec run t play teams = match t with
  | [] -> [teams]
  | hd :: tl ->
    let n = Proper.number_of_teams hd in
    let top, bot = List.top_of_list n teams in
    let results = Proper.run hd play top in
    List.hd results :: run tl play (List.flatten (List.tl results) @ bot)
;;





type triviality =
  | Anything
  | NoSemitrivial
  | Efficient
  | NoTrivial
;;

let get_all
    ?(respectfulness = Tier.Not)
    ?(triviality = Anything)
    ?(max_games = Int.max_int)
    (input_tiers : Tier.t list)
    (output_tiers : int list)
  = 
  let rec f input_tiers output_tiers =
    match input_tiers, output_tiers with
    | _, [] -> assert false 
    | _, [0] -> [[]]
    | [], _ -> assert false
    | _, m :: tl ->
        let triviality = match triviality, m with
          | Efficient, 0 -> NoSemitrivial
          | Efficient, _ -> NoTrivial
          | _, _ -> triviality
        in
        let m, tl = if m = 0 then List.pop tl else m, tl in
        Proper.get_all ~max_games ~max_target_sum:m ~include_smaller:true ~respectfulness ~include_semitrivial:(triviality = Anything) ~include_trivial:(triviality <> NoTrivial) input_tiers 
        |> List.map (fun (bracket, output_tiers) -> bracket, f (List.pop_last output_tiers) ((m - Proper.number_of_winners bracket) :: tl))
        |> List.map (fun (bracket, lst) -> List.map (List.cons bracket) lst)
        |> List.flatten
  in
  f input_tiers (0 :: output_tiers)
;;