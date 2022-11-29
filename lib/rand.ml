let set_seed () = Random.init @@ Float.to_int @@ Unix.gettimeofday() *. 1000.;;

let rec get_gaussian() : float =
  let x = Random.float 2.0 -. 1.0 in
  let y = Random.float 2.0 -. 1.0 in
  let s = x*.x +. y*.y in
  if s > 1.0 then get_gaussian()
  else x *. sqrt (-2.0 *. (log s) /. s)
;;