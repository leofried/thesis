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
  val get : t -> int list
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
  let get = Fun.id
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


let f (tiers : T.t) (bracket : Proper.t) (odds : float) (games_played : ((int * int) * (int * int)) list) =
  tiers
  |> T.get
  |> List.map List.create
  |> List.mapi (fun i -> List.map (Pair.pair i))
  |> List.map Combo.permutations
  |> Combo.combinations
  |> List.map List.flatten
  |> Debug.print (List.sexp_of_t (List.sexp_of_t (Pair.sexp_of_t sexp_of_int sexp_of_int)) >> Sexp.to_string)
  |> List.map (fun lst -> List.map (Tree.map_und (List.nth_one lst)) (Proper.build_brackets bracket))
  |> List.map (List.map (Tree.map_und (fun x -> Discrete.single (x, 0))))
  |> List.map (List.map (Tree.fold (fun (d1 : ((int * int) * int) Discrete.t) d2 ->
    Discrete.cross d1 d2
    |> Discrete.map (fun (((t1, i1), r1), ((t2, i2), r2)) ->
      let odds = if t1 < t2 then odds else if t1 > t2 then 1. -. odds else 0.5 in
      let r3 = r1 + r2 + if List.mem ((t1, i1), (t2, i2)) games_played || List.mem ((t2, i2), (t1, i1)) games_played then 1 else 0 in
      [((t1, i1), r3), odds; ((t2, i2), r3), 1. -. odds]
    )
    |> Discrete.collapse_horizontal
    |> Discrete.collapse_verticle
  )))
  |> List.map (List.map (Discrete.map (Pair.right >> float_of_int)))
  |> List.map (List.map Discrete.expect)
  |> List.map (List.fold_left (+.) 0.)
;;