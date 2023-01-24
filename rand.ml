let hidden = ref None;;

let rec get_gaussian() : float =
  match !hidden with
  | None ->
    let x = Random.float 2.0 -. 1.0 in
    let y = Random.float 2.0 -. 1.0 in
    let s = x*.x +. y*.y in
    if s > 1.0 then get_gaussian() else
      let c = sqrt (-2.0 *. (log s) /. s) in
      hidden := Some (x *. c);
      y *. c 
  | Some r ->
    hidden := None;
    r
;;