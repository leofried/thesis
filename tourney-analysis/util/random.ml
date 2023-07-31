open! Std

module R = Stdlib.Random;;

let () =
  Unix.gettimeofday()
  |> ( *. ) 1000.
  |> Float.to_int
  |> R.init
;;

type f =
  | Normal of float * float
  | Uniform of float * float
  | Floored of float * f
[@@deriving sexp];;

let gaussian = Normal (0., 1.);;

let next_gaussian = ref None;;

let rec next_float f = match f with
  | Normal (mean, stdev) ->
    begin match !next_gaussian with
    | None ->
      let x = R.float 2.0 -. 1.0 in
      let y = R.float 2.0 -. 1.0 in
      let s = x*.x +. y*.y in
      if s > 1.0 then next_float gaussian else
        let c = sqrt (-2.0 *. (log s) /. s) in
        next_gaussian := Some (x *. c);
        (y *. c)
    | Some r ->
        next_gaussian := None;
        r
    end
    *. stdev +. mean

  | Uniform (x, y) -> R.float (x -. y) +. y

  | Floored (x, g) ->
    let r = next_float g in
    if x <= r then r else next_float f
;;

let bool = R.bool;;

let shuffle lst =
  let nd = List.map (fun c -> (R.bits (), c)) lst in
  let sond = List.sort compare nd in
  List.map snd sond
;;