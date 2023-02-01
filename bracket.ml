let rec convert (bracket : int list) (i : int) (arr : int Tree.t array) : unit =
  match bracket with
  | [1] -> ()
  | [] | [_] -> invalid_arg "Bracket.convert"
  | 0 :: tl -> convert tl i arr
  | x :: _ when x < 0 -> invalid_arg "Bracket.convert"
  | a :: b :: tl -> 
    let j = i - a + 1 in
    arr.(j) <- Branch (arr.(j), arr.(i));
    convert (a - 2 :: b + 1 :: tl) (i - 1) arr
;;

let build_tree (bracket : int list) : int Tree.t =
  let n = List.fold_left ( + ) 0 bracket in
  let arr = Array.init n (fun i -> Tree.Leaf i) in
  convert bracket (n - 1) arr;
  arr.(0)
;;

let rec run_bracket (tree : int Tree.t) (teams : Team.t list) : Team.t list =
  match tree with
  | Leaf i -> [List.nth teams i]
  | Branch (i, j) ->
    let top = run_bracket i teams in
    let bot = run_bracket j teams in
    let winner, loser = Team.play_game (List.hd top) (List.hd bot) in
    winner :: loser :: (List.tl top) @ (List.tl bot)
;;

let rec count_games (bracket : int list) : int =
  match bracket with
  | 0 :: tl -> count_games tl
  | _ -> List.length bracket - 1
;;

let rec is_fair (bracket : int list) (seed_size : int) : bool =
  match bracket with
  | [] -> true
  | hd :: tl -> if hd mod seed_size = 0 then is_fair tl seed_size else false
;;

type argument = string;;

let stb str = List.map int_of_string (String.split_on_char '_' str);;

let number_of_teams (bracket : argument) = List.fold_left ( + ) 0 (stb bracket);;

let name (bracket : argument) = Int.to_string (number_of_teams bracket) ^ " team " ^ Lists.to_string Int.to_string (stb bracket) ^ "-bracket";;

let max_games (bracket : argument) = count_games (stb bracket);;

let is_fair (bracket : argument) = is_fair (stb bracket) (number_of_teams bracket);;

let run (bracket : argument) = run_bracket (build_tree (stb bracket));;

let kind = "bracket";;
(*
let make ~(bracket : int list) : Scheme.t =
  let number_of_teams = List.fold_left ( + ) 0 bracket in
  {
    name = Int.to_string number_of_teams ^ " team " ^ Lists.to_string Int.to_string bracket ^ "-bracket";
    number_of_teams;
    max_games = count_games bracket;
    is_fair = is_fair bracket number_of_teams;
    run = run_bracket (build_tree bracket);
  }
;;  

let rec bracket_children (bracket : int list) : int list list =
  match bracket with
  | [] -> []
  | hd :: tl -> 
    let lst = List.map (fun lst -> 0 :: hd + List.hd lst :: List.tl lst) (bracket_children tl) in
    if hd = 0 then lst else (2 :: (hd - 1) :: tl) :: lst
;;

let rec get_all_brackets (number_of_teams : int) : int list list list =
  match number_of_teams with
  | 1 -> [[[1]]]
  | n ->
    let lst = get_all_brackets (n-1) in
    lst
    |> List.hd
    |> List.map bracket_children
    |> List.flatten
    |> List.sort_uniq (List.compare Int.compare)
    |> Fun.flip List.cons lst
;;

let eliom : (string, Param_specs.one_string) Scheme.eliom_builder = 
  {
    name = "bracket";
    params = Param_specs.String "bracket";
    make = fun (bracket) -> make ~bracket:(List.map int_of_string (String.split_on_char '_' bracket));
  }
  *)