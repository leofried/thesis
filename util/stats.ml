type t = {
  samples : int;
  sum : float;
  sum_squares : float;
} [@@deriving yojson];;

let empty = {
  samples = 0;
  sum = 0.;
  sum_squares = 0.;
}

let of_list = function
  | [] -> empty
  | lst -> {
    samples = List.length lst;
    sum = Lists.fold (+.) lst;
    sum_squares = Lists.fold (+.) (List.map (fun x -> x ** 2.) lst)
  }
;;

let combine s1 s2 = {
  samples = s1.samples + s2.samples;
  sum = s1.sum +. s2.sum;
  sum_squares = s1.sum_squares +. s2.sum_squares;
}

let mean {samples; sum; _}= Math.divide_float_int sum samples;;

let stdev {samples; sum; sum_squares} = 
  sqrt (Math.divide_float_int (sum_squares -. (Math.divide_float_int (sum ** 2.) samples)) (samples - 1))
;;

let stderr ({samples; _} as stats) = stdev stats /. (Math.sqrt_int samples);;


(*
let mean (lst : float list) : float =
  Math.divide_float_int (Lists.fold (+.) lst) (List.length lst)
;;

let stdev (lst : float list) : float =
  let mean = mean lst in
  lst
  |> List.map (fun x -> (x -. mean) ** 2.0)
  |> Lists.fold (+.)
  |> Fun.flip Math.divide_float_int (List.length lst - 1)
  |> sqrt
;;

let stderr (lst : float list) : float = stdev lst /. Math.sqrt_int (List.length lst);;

let normed_stdev (lst : float list) : float = (stdev lst) /. (Lists.fold (+.) lst);;

let mean_two (n1, mean1 : int * float) (n2, mean2 : int * float) : float =
  let m1 = Int.to_float n1 in
  let m2 = Int.to_float n2 in
  (mean1 *. m1 +. mean2 *. m2 ) /. (m1 +. m2)
;;

let stdev_two (n1, mean1, stdev1 : int * float * float) (n2, mean2, stdev2 : int * float * float) : float =
  let m1 = Int.to_float n1 in
  let m2 = Int.to_float n2 in
  sqrt (((m1 -. 1.) *. stdev1 *. stdev1 +. (m2 -. 1.) *. stdev2 *. stdev2) /. (m1 +. m2 -. 1.) +.
  m1 *. m2 *. (mean1 -. mean2) *. (mean1 -. mean2) /. (m1 +. m2) /. (m1 +. m2 -. 1.))
;;

let stderr_two (n1, mean1, stderr1 : int * float * float) (n2, mean2, stderr2 : int * float * float) : float =
  let m1 = Int.to_float n1 in
  let m2 = Int.to_float n2 in
  stdev_two (n1, mean1, stderr1 *. sqrt m1) (n2, mean2, stderr2 *. sqrt m2) /.
  (sqrt (m1 +. m2))
;;

let binom_error_monte_carlo ~(accuracy : int) ~(iters : int) ~(cats : int) : float = 
  let rec f = function
    | 0 -> []
    | x ->
      let arr = Array.make cats 0 in
      for _ = 1 to iters do
        Math.inc_array arr (Random.int cats)
      done;
      normed_stdev (List.map Int.to_float (Array.to_list arr)) :: f (x - 1)
  in
  let data = f (Math.pow 10 accuracy) in
  mean data;;
;;

let binom_error_formula ~(iters : int) ~(cats : int) : float = 1. /. Math.sqrt_int (iters * cats);;


let sample (n : int) (lst : ('a * float) list) : 'a list =
  let rec take k = function
    | [] -> assert false
    | (v, p) :: tl -> if k < p then v else take (k -. p) tl
  in let rec f n lst =
    match n with
    | 0 -> []
    | _ ->
      let tot = Lists.fold (+.) (snd (List.split lst)) in
      take (Random.float tot) lst :: (f (n - 1) lst)
  in f n lst
;;*)