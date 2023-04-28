open Util;;
open Infix;;
open Engine;;

module S = struct
  type t = {
    number_of_teams : int;
    number_of_pools : int;
    auto_bids : int;
    brackets : Bracket.t list
  } [@@deriving yojson];;

  let kind = "symm_multi";;

  let to_string {number_of_teams; number_of_pools; auto_bids; brackets} =
    (string_of_int number_of_teams) ^ " teams -> " ^
    (string_of_int number_of_pools) ^ " pools -> {" ^
    (string_of_int auto_bids) ^ "} -> " ^
    (String.concat " -> " (List.map (Lists.to_string string_of_int) brackets))
  ;;


  let run {number_of_pools; auto_bids; brackets; _} teams =
    let autos, pools =
      Pools.run number_of_pools teams
      |> List.map (Lists.top_of_list (auto_bids / number_of_pools))
      |> List.split
    in

    let rec f pools = function
      | [] -> Lists.unwind pools
      | c :: colors -> 
         List.hd (List.nth pools c) ::
         (f (List.mapi (fun i -> if i = c then List.tl else Fun.id) pools) colors)
    in

    let teams = if brackets = [] then (List.flatten pools) else f pools (Tree.color (Bracket.to_tree (List.hd brackets)) number_of_pools) in

    let teams, advs = List.fold_left_map 
      (fun teams bracket ->
        Bracket.run_bracket bracket teams
        |> Lists.top_of_list (Bracket.count_advance bracket)
        |> Tuple.swap
      )
      teams
      brackets
    in List.concat [List.flatten autos; List.flatten advs; teams]
  ;; 
end

include S;;
module Data = Data.M (S);;

let count_advance {auto_bids; brackets; _} =
  auto_bids + Lists.fold (+) ~def:0 (List.map Bracket.count_advance brackets);;
;;

let get_tiers {number_of_teams; number_of_pools; auto_bids; brackets} : int * Tiers.t =
  assert (number_of_teams mod number_of_pools = 0);
  assert (auto_bids mod number_of_pools = 0);
  List.fold_left
    (fun (adv, tiers) bracket -> 
      assert (Tiers.compatable tiers (Bracket.input_tiers bracket));
      Tuple.apply ((+) adv >>@ List.hd, List.tl) (Tiers.compose tiers (Bracket.output_tiers bracket))
    )
    (auto_bids, List.init ((number_of_teams - auto_bids) / number_of_pools) (fun _ -> number_of_pools))
    brackets
;;

let get_all (specs : Specs.t) =
  let shells =
    specs.number_of_teams
    |> Math.divisors
    |> List.filter (fun number_of_pools -> specs.number_of_teams / number_of_pools - 1 <= specs.max_games)
    |> List.map (fun number_of_pools -> List.init (1 + specs.number_advance / number_of_pools) (fun k -> {
        number_of_teams = specs.number_of_teams;
        number_of_pools;
        auto_bids = k * number_of_pools;
        brackets = [];
    }))
    |> List.concat
  in

  let rec f base =
    let advance, tiers = get_tiers base in
    if advance = specs.number_advance then [base] else
    List.init (specs.number_advance - advance) ((+) 1)
    |> List.map (fun target_sum -> Bracket.get_all_brackets
      ~max_games: (base.number_of_teams / base.number_of_pools - 1)
      ~target_sum
      ~tiers
      ~require_games: true
    )
    |> List.flatten
    |> List.map (fun bracket -> {base with brackets = base.brackets @ [bracket]})
    |> List.filter (fun s -> Data.max_games specs.number_of_teams s <= specs.max_games)
    |> List.map f
    |> List.flatten
  in

  List.flatten (List.map f shells)
;;