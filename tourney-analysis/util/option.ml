module O = Stdlib.Option;;

type 'a t = 'a option;;

let fold none some t = O.fold ~none ~some t;;

let bind t f = O.bind f t;;

let map = O.map;;

let is_some = O.is_some;;