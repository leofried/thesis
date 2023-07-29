open! Std;;

type t = {
  samples : int;
  sum : float;
  sum_squares : float;
} [@@deriving sexp];;

let empty = {
  samples = 0;
  sum = 0.;
  sum_squares = 0.;
}

let add_sample t (x : float) = {
  samples = t.samples + 1;
  sum = t.sum +. x;
  sum_squares = t.sum_squares +. x *. x;
}

let combine t1 t2 = {
  samples = t1.samples + t2.samples;
  sum = t1.sum +. t2.sum;
  sum_squares = t1.sum_squares +. t2.sum_squares;
}

let count {samples; _} = samples;;

let mean {samples; sum; _} = Math.divide_float_int sum samples;;

let stdev {samples; sum; sum_squares} = 
  sqrt (Math.divide_float_int (sum_squares -. (Math.divide_float_int (sum ** 2.) samples)) (samples - 1))
;;

let stderr ({samples; _} as stats) = stdev stats /. (Math.sqrt_int samples);;

