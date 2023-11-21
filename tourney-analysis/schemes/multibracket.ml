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



let rec get_all_complete_simple = function
  | 0 -> [[]]
  | n ->
    let all_propers = Proper.get_all n in
    let all_multis = get_all_complete_simple (n - 1) in
    all_propers
    |> List.map (fun proper -> List.map (fun multi -> proper :: multi) all_multis)
    |> List.flatten
;;
    