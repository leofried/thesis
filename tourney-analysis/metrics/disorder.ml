open! Util;;
open! Std;;

type t = int [@@deriving sexp];;
type u = int list [@@deriving sexp];;

let kind = "disorder";;

let empty n = List.create n |> List.map (fun _ -> 0);;

let fold (_ : t) u ~(teams:Team.t list) ~(results:Team.t list) =
  let i = 
    results
    |> List.hd
    |> List.find (Team.sort teams)
  in List.mapi (fun j x -> if i = j then x + 1 else x) u
;;

let combine (_ : t) u = List.combine u >> List.map (Pair.uncurry (+));;

let score (_ : t) u =
  print_endline (Sexp.to_string (sexp_of_u u));
  let x = Math.divide_int_int
    (List.fold_downstream (fun score hd tl ->
      List.fold_left (fun score tl ->
        score + if hd < tl then tl - hd else 0
      ) score tl
    ) 0 u)
    (List.fold_left (+) 0 u)
  in
  print_endline (string_of_float x);
  x , 0.
;;
