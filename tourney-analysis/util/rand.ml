let set_seed () = Random.init @@ Float.to_int @@ Unix.gettimeofday() *. 1000.;;

let next_gaussian = ref None;;

let rec get_gaussian() : float =
  match !next_gaussian with
  | None ->
    let x = Random.float 2.0 -. 1.0 in
    let y = Random.float 2.0 -. 1.0 in
    let s = x*.x +. y*.y in
    if s > 1.0 then get_gaussian() else
      let c = sqrt (-2.0 *. (log s) /. s) in
      next_gaussian := Some (x *. c);
      y *. c 
  | Some r ->
      next_gaussian := None;
      r
;;

let shuffle lst =
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
  let sond = List.sort compare nd in
  List.map snd sond
;;