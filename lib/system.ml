exception ImplementationError
let error () = raise ImplementationError

let line () = print_endline "";;

let time (f : unit -> unit) = print_endline (Float.to_string (Unix.times (f ())).tms_utime ^ " seconds.");;

(*
let rec pow a = function
| 0 -> 1
| 1 -> a
| n -> 
  let b = pow a (n / 2) in
  b * b * (if n mod 2 = 0 then 1 else a)
;;P

let rec apply f n x = 
  match n with 
  | 0 -> x
  | _ -> apply f (n - 1) (f x)
;;

let set_seed () = Random.init @@ Float.to_int @@ Unix.gettimeofday() *. 1000.;;

let rec get_gaussian() : float =
  let x = Random.float 2.0 -. 1.0 in
  let y = Random.float 2.0 -. 1.0 in
  let s = x*.x +. y*.y in
  if s > 1.0 then get_gaussian()
  else x *. sqrt (-2.0 *. (log s) /. s)
;;

let inc (r : int ref) : unit = r := !r + 1;;

let divide (x : int) (y : int) : float = Int.to_float x /. Int.to_float y;;

let shuffle (lst : 'a list) : 'a list =
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
  let sond = List.sort compare nd in
  List.map snd sond
;;



*)