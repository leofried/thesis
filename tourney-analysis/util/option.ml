open! Std;;

module O = Stdlib.Option;;

type 'a t = 'a option;;

let fold some t none = O.fold ~none ~some t;;

let bind t f = O.bind f t;;

let map = O.map;;
