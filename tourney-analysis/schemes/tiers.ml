open! Util
open! Std
open! Struct

(* 
type s =
  (* | Strongly (*must be completely identical*)
  (* | Weakly   must be completely identical up until chalked loss
  | Byely    (*must have the same number of byes*)*)*)
  | None     (*no restrictions*)
  | Rounds
;; *)


type s =
  | Any
  | Byes
  | Strong
;;

module T : sig
  type t
  val make : int list -> t
  val length : t -> int
  val number_of_teams : t -> int
  val empty : t
  val pop : t -> int * t
  val pop_all : int -> t -> t * t
  val add_top : int -> t -> t
  val add_bot : int -> t -> t
  val add_all : t -> t -> t
  val rev : t -> t
end = struct
  type t = int list
  let make = Fun.id
  let length = List.length
  let number_of_teams = List.fold_left (+) 0
  let empty = []
  let pop = List.pop
  let pop_all = List.top_of_list
  let add_top = List.cons
  let add_bot x a = a @ [x]
  let add_all = List.append
  let rev = List.rev
end 


let rec pull (t, bl) k = 
  match k with
  | 0 -> (t, bl), T.empty
  | _ -> 
    let hd, tl = T.pop t in
    if hd > k then 
      (T.add_top (hd - k) tl, false), T.add_top k T.empty 
    else 
      pull (tl, bl) (k - hd)
      |> Pair.map_right (T.add_top hd)
;;

let split_symm t =
  let a, b = T.pop_all (T.length t / 2) t in
  if T.length t mod 2 = 0 then
    if T.rev a = b then Some a else None
  else
    let x, c = T.pop b in
    if T.rev a = c then Some (T.add_bot (x / 2) a) else None
;;


let check_proper_bracket s t bracket = 
  let (_, bl), tlist = Pair.map_right List.rev (List.fold_left_map pull (t, true) (List.rev bracket)) in
  match s with
  | Any -> true
  | Byes -> bl
  | Strong -> bl &&
    tlist
    |> List.fold_left (fun o t ->
      match o with
      | None -> None
      | Some o -> split_symm (T.add_all t o)
    ) (Some T.empty)
    |> Option.is_some
;;
    


(*
let assign_tiers_to_bracket ~tiers ~bracket =
  bracket
  |> List.rev
  |> List.fold_left_map (fun tiers n ->
    let rec f k = function
    | []
    
    )





 let rec check_proper_bracket t s b = match s with
  | Rounds ->
    match b with
    | [] -> print_endline "a"; assert false
    | [n] -> number_of_teams t = n
    | n1 :: n2 :: bracket ->
      match t with
      | [] ->  print_endline "a";  print_endline "a"; assert false
      | hd1 :: t ->
        if hd1 > n1 then false else
        if hd1 = n1 then check_proper_bracket ((hd1 / 2) :: t) Rounds (n2 + hd1 / 2 :: bracket) else
          match t with
          | [] ->  print_endline "a";  print_endline "a";  print_endline "a"; assert false
          | hd2 :: t -> 
            check_proper_bracket (hd1 + hd2 :: t) Rounds b
;; *)







(* 
let get_all
  ?(max_games = None)
  ?(target_sum = 1) 
  ?(require_games = false)
  tie
: Proper.t list =  
  let rec f max_games target_sum curr tiers : t list =
    if target_sum = 0 then [[curr]] else
    if target_sum < 0 then [] else
    if Option.fold ((>=) 0) max_games false then [] else 
      let hds = List.map (List.cons curr) (f (Option.map (Fun.flip (-) 1) max_games) (target_sum * 2) 0 tiers) in
      if tiers = [] then hds else 
        let next, new_tiers = List.pop tiers in
        hds @ f max_games (target_sum - next) (curr + next) new_tiers
          
  in List.map List.rev (
    if require_games then (List.map (List.cons 0) (f (Option.map (Fun.flip (-) 1) max_games) (target_sum * 2) 0 tiers))
    else f max_games target_sum 0 tiers
  )
;; *)