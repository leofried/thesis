open! Std

include Num

type t = num;;

let sexp_of_t = float_of_num >> sexp_of_float;;

let t_of_sexp _ = assert false;;


let zero = num_of_int 0
let one = num_of_int 1
let frac a b = Num.div_num (num_of_int a) (num_of_int b)
