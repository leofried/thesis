let mean (lst : float list) : float =
  Math.divide_float_int (List.fold_left Float.add 0. lst) (List.length lst)
;;

let stdev (lst : float list) : float =
  let mean = mean lst in
  lst
  |> List.map (fun x -> (x -. mean) ** 2.0)
  |> List.fold_left Float.add 0.
  |> Fun.flip Math.divide_float_int (List.length lst - 1)
  |> sqrt
;;

let stderr (lst : float list) : float = stdev lst /. Math.sqrt_int (List.length lst);;

let normed_stdev (lst : float list) : float = (stdev lst) /. (List.fold_left Float.add 0. lst);;

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

let binom_error ?(accuracy : int = 3) ~(iters : int) ~(cats : int) () : float = 
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


let sample (n : int) (lst : ('a * float) list) : 'a list =
  let rec take k = function
    | (v, p) :: tl -> if k < p then v else take (k -. p) tl
    | _ -> System.error ()
  in let rec f n lst =
    match n with
    | 0 -> []
    | _ ->
      let tot = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 lst in
      take (Random.float tot) lst :: (f (n - 1) lst)
  in f n lst
;;