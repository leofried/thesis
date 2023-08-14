open! Std;;

type t = {
  samples : int;
  sums : float list;
  prods : float list list;
} [@@deriving sexp];;

let empty k = 
  let zeros = List.map (Fun.const 0.) (List.create k) in
  {
    samples = 0;
    sums = zeros;
    prods = List.map (Fun.const zeros) zeros;
  }
;;

(*
let of_list = function
  | [] -> empty
  | lst -> {
    samples = List.length lst;
    sums = List.fold_left (+.) 0. lst;
    sum_squares = List.fold_left (+.) 0. (List.map (fun x -> x ** 2.) lst)
  }
;; *)

let combine t1 t2 = {
  samples = t1.samples + t2.samples;
  sums = List.combine_map (+.) t1.sums t2.sums;
  prods = List.combine_map (List.combine_map (+.)) t1.prods t2.prods;
}

let add_sample x = 
  let rec cross = function
  | [] -> []
  | hd :: tl as lst ->
    let mul = List.map (( *. ) hd) in
    mul lst :: List.combine_map List.cons (mul tl) (cross tl)
  in
  
  combine {
    samples = 1;
    sums = x;
    prods = cross x
  }
;;

let size t = List.length t.sums;;

let samples {samples; _} = samples;;

let means {samples; sums; _} = List.map (Fun.flip Math.divide_float_int samples) sums;;

let covars {samples; sums; prods} =
  List.mapi (fun i lst ->
    List.mapi (fun j prod ->
      Math.divide_float_int (
        prod -. (
          Math.divide_float_int (
            (List.nth sums i) *. (List.nth sums j)
          ) samples
        )
      ) (samples - 1)
    ) lst
  ) prods
;;
  

let weight weights t = {
  samples = t.samples;
  sums = List.combine_map ( *. ) weights t.sums;
  prods = 
    t.prods
    |> List.map (List.mapi (fun i -> ( *. ) (List.nth weights i)))
    |> List.mapi (fun i -> List.map (( *. ) (List.nth weights i)))
  ;
}

let collapse t =
  let sum = List.fold_left (+.) 0. t.sums in
  {
    samples = t.samples;
    sums = [sum];
    prods = [[
      t
      |> covars
      |> List.map (List.fold_left (+.) 0.)
      |> List.fold_left (+.) 0.
      |> Math.mul_int_float (t.samples - 1)
      |> (+.) (Math.divide_float_int (sum *. sum) t.samples)
    ]];
  }
;;

let stdevs t =
  let covars = covars t in
  size t
  |> List.create
  |> List.map (fun i -> List.nth (List.nth covars i) i)
  |> List.map sqrt
;;

let stderrs t = List.map (Fun.flip (/.) (Math.sqrt_int t.samples)) (stdevs t);;

