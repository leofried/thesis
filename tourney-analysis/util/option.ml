module O = Stdlib.Option;;

type 'a t = 'a option;;

let fold none some t = O.fold ~none ~some t;;

let bind f t = O.bind t f;;

let map = O.map;;

let is_some = O.is_some;;

let join = O.join;;

let some = O.some;;

let apply t = fold Fun.id Fun.id t;;