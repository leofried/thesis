open! Util
open! Std
open! Schemes

type t = int list list;;

module T = struct
  type s = 
    | D of int list
    | C of float list list

  type t = int Discrete.t

  let get_p s i1 i2 =
    let i1 = i1 - 1 in
    let i2 = i2 - 1 in
    match s with
    | D s ->
      if i1 < i2 then
        if List.nth s i1 < i2 then 1. else 0.5
      else
      if List.nth s i2 < i1 then 0. else 0.5
    | C s -> List.nth (List.nth s i1) i2
    

  let play_game (s : s) (t1 : t) (t2 : t) : t * t =
    Discrete.cross t1 t2
    |> Discrete.map (fun (i1, i2) -> Discrete.pair (get_p s i1 i2) i1 i2)
    |> Discrete.flatten
    |> Pair.rev (Discrete.single ~-1)
end


let f0 ~bracket ~scores ~(tiers : t) =
    tiers
    |> Debug.mark "a"
    |> List.map Combo.permutations
    |> Debug.mark "a"
    |> Combo.combinations
    |> Debug.mark "a"
    |> List.map List.flatten
    |> List.map (fun teams ->
      Scheme.run 
        (Scheme.create (module Proper) bracket)
        (T.play_game scores)
        (List.map Discrete.single teams)
      |> List.hd
    )
    |> Discrete.uniform
    |> Discrete.flatten
    |> Debug.print (Discrete.sexp_of_t sexp_of_int >> Sexp.to_string)
    |> Discrete.ordered Int.compare
;;

let r = ref 0

let f1 ~bracket ~(tiers : t) =
  print_endline (Ref.incr r |> string_of_int);
  print_endline ((sexp_of_list (sexp_of_list sexp_of_int)) tiers |> Sexp.to_string);
  bracket
  |> Proper.number_of_teams
  |> Debug.mark "c"
  |> Combo.ssts
  |> Debug.mark "b"
  |> List.map (fun s -> T.D s)
  |> List.for_all (fun scores -> f0 ~bracket ~scores ~tiers)
  |> Debug.print (string_of_bool)
  |> Debug.hold (fun x -> if x then print_endline "----------------------------xxx" else ())
  |> Debug.mark "---"
;;


let f2 ~bracket =
  Proper.number_of_teams bracket
  |> Combo.compositions
  |> List.sort_by_rev (List.length) Int.compare
  |> Debug.print (List.length >> Int.to_string) 
  |> List.filter (fun tiers -> f1 ~bracket ~tiers)
  |> Debug.print (List.length >> Int.to_string) 
  |> List.sexp_of_t (List.sexp_of_t (List.sexp_of_t sexp_of_int))
  |> Sexp.to_string
  |> print_endline