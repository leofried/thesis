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

let binom_error ?(accuracy : int = 3) ~(iters : int) ~(cats : int) () : float = 
  let rec f = function
    | 0 -> []
    | x ->
      let arr = Array.make cats 0 in
      for _ = 1 to iters do
        Math.inc_array arr (Random.int cats)
      done;
      normed_stdev (List.map Int.to_float (Array.to_list arr)) :: f (x - 1)
  in mean (f (Math.pow 10 accuracy))
;;
    
