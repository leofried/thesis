module O = Stdlib.Option;;

type 'a t = 'a option;;

(*clean this lol*)

let fold none some t = O.fold ~none ~some t;;

let set none = function
  | None -> none
  | Some x -> x
;; 

let bind f t = O.bind t f;;

let map = O.map;;

let is_some = O.is_some;;

let join = O.join;;

let some = O.some;;

let apply t = fold Fun.id Fun.id t;;