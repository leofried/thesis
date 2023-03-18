open Util;;
open Infix;;

type t = Adic.t list list;;

let transform grid =
  let base = Lists.fold (+) (List.map (Lists.fold (+)) grid) in
  List.map (List.map (fun x -> Adic.of_float (Math.divide_int_int x base))) grid
;;

let verify grid =
  if 
    Adic.equals (Adic.of_float 1.) (Lists.fold Adic.add (List.map (Lists.fold Adic.add) grid))
  && 
    List.for_all (
        List.for_all (fun (x, y) ->
          Adic.to_float (Tuple.right (Adic.split_perfect x)) >= Adic.to_float (Tuple.left (Adic.split_perfect y))
        ) >>@ Lists.pair_offset
      ) grid
  &&
    List.fold_left (fun opt v -> match opt with
      | None -> None
      | Some None -> let (x, y) = Adic.split_perfect v in if x = y then Some None else Some (Some v)
      | Some Some w -> if v = w then Some None else None
    ) (Some None) (List.sort Adic.compare (List.flatten grid)) = Some None
  then () else assert false
;;


  



