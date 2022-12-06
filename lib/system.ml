exception ImplementationError
let error () = raise ImplementationError

let line () = print_endline "";;

let time (f : unit -> 'a) : 'a * float =
  let time_start = Unix.time () in
  let value = f () in
  let time_end = Unix.time () in
  value, (time_end -. time_start)
;;
