open! Util
open! Std
open! Schemes

type t = int list list;;

module T = struct
  type s = 
    | D of int list
    (* | C of Num.t list list *)
    [@@deriving sexp]

  type t = int Discrete.t

  let p = Rat.frac 1 4
  let q = Rat.sub_num Rat.one p


  let get_p s i1 i2 =
    let i1 = i1 - 1 in
    let i2 = i2 - 1 in
    match s with
    | D s ->
      if i1 < i2 then
        if List.nth s i1 < i2 then q else Rat.frac 1 2
      else
      if List.nth s i2 < i1 then p else Rat.frac 1 2
    (* | C s -> List.nth (List.nth s i1) i2 *)
    

  let play_game (s : s) = Game.{
    tiebreaker = Random;
    compare = Fun.const (Fun.const 0); 
    play = (fun t1 t2 ->
    Discrete.cross t1 t2
    |> Discrete.map ~eq:Discrete.equal (fun (i1, i2) -> Discrete.pair (get_p s i1 i2) i1 i2)
    |> Discrete.flatten
    |> Pair.rev (Discrete.single ~-1)
  )}
end



let f0 ~bracket ~scores ~(tiers : t) =
    tiers
    |> List.map Combo.permutations
    |> Combo.combinations
    |> List.map List.flatten
    |> List.map (fun teams ->
      Scheme.run 
        (Scheme.create (module Proper) bracket)
        (T.play_game scores)
        (List.map Discrete.single teams)
      |> List.hd
    )
    |> Discrete.uniform ~eq:Discrete.equal
    |> Discrete.flatten
    |> (fun d ->
      let o = Discrete.ordered Int.compare d in
      print_endline (T.sexp_of_s scores |> Sexp.to_string);
      if o then () else begin
        print_endline (Discrete.sexp_of_t sexp_of_int d |> Sexp.to_string)
      end; o
    )

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
  (* |> List.map (fun scores -> f0 ~bracket ~scores ~tiers) *)
  |> List.for_all (fun scores -> f0 ~bracket ~scores ~tiers)
  |> Debug.print (string_of_bool)
  |> Debug.hold (fun x -> if x then print_endline "----------------------------xxx" else ())
  |> Debug.mark "---"
;;

(* 
let f2 ~bracket =
  Proper.number_of_teams bracket
  |> Combo.compositions
  |> List.sort_by_rev (List.length) Int.compare
  |> Debug.print (List.length >> Int.to_string) 
  |> List.filter (fun tiers -> f1 ~bracket ~tiers)
  |> Debug.print (List.length >> Int.to_string) 
  |> List.sexp_of_t (List.sexp_of_t (List.sexp_of_t sexp_of_int))
  |> Sexp.to_string
  |> print_endline *)