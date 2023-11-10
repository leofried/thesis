
module Prob = struct
  type t = float list
      
  let neg = List.map (fun x -> ~-.x)

  let rec add prob1 prob2 = match prob1, prob2 with 
    | _, [] -> prob1
    | [], _ -> prob2
    | hd1 :: tl1, hd2 :: tl2 -> hd1 +. hd2 :: add tl1 tl2 
                                  
  let mul prob1 prob2 =
    let rec to_add prob1 prob2 =
      match prob1 with
      | [] -> []
      | hd1 :: tl1 ->
          List.map (( *. ) hd1) prob2 ::
          List.map (fun lst -> 0. :: lst) (to_add tl1 prob2)
    in List.fold_left add [] (to_add prob1 prob2)

  let zero = [0.]
  let half = [0.5]
  let one = [1.]
  let p = [0.; 1.]
  let p2 = [0.; 0.; 1.] 
           
  let one_minus prob = add one (neg prob)
  let q = one_minus p
  let q2 = one_minus p2
end

let rec grab_from_list lst = function
  | 0 -> List.hd lst, List.tl lst
  | n -> 
      let item, new_lst = grab_from_list (List.tl lst) (n-1) in
      item, List.hd lst :: new_lst
;;


module Sig = struct
  type t = int list
  
  let succ lst = (List.hd lst / 2) + (List.hd (List.tl lst)) :: List.tl (List.tl lst) 
                   
  let rec matchups teams bracket : (int * int) list * int list = 
    match List.hd bracket with
    | 0 -> [], teams
    | n -> 
        let a, teams = grab_from_list teams (List.length teams - n) in
        let b, teams = grab_from_list teams (List.length teams - 1) in
        let matchs, rest = matchups teams ((n-2) :: List.tl bracket) in
        (a, b) :: matchs, rest 
end

let f ~(table : int -> int -> Prob.t) ~(bracket : Sig.t) : Prob.t list = 
  let rec f ~n ~table ~alive ~bracket =
    if bracket = [1] then (List.init n (fun i -> if alive = [i+1] then Prob.one else Prob.zero)) else
      
      let alive = List.sort compare alive in
      let matchups, rest = Sig.matchups alive bracket in
    
      let add_matchup (probs : (int list * Prob.t) list) (a, b) : (int list * Prob.t) list = 
        List.map (fun (alive, prob) -> [a :: alive, Prob.mul prob (table a b); b :: alive, Prob.mul prob (table b a)]) probs
        |> List.flatten
      in
    
      matchups
      |> List.fold_left add_matchup [rest, Prob.one]
      |> List.map (fun (teams, prob) -> List.map (Prob.mul prob) (f ~n ~table ~alive:teams ~bracket:(Sig.succ bracket)))
      |> List.fold_left (List.map2 Prob.add) (List.init n (fun _ -> Prob.zero))
  in f ~n:(List.fold_left (+) 0 bracket) ~bracket ~table ~alive:(List.init (List.fold_left (+) 0 bracket) (fun i -> i +1))
;;
  
(* 
let rec table1 i j = 
  if i > j then Prob.one_minus (table1 j i) else
    match (i, j) with 
    | 3, 4 | 3, 5 | 4, 5 | 4, 6 | 4, 7 | 5, 6 | 5, 7 | 6, 7 -> Prob.half 
    | _, 8 -> Prob.one
    | _, _ -> Prob.q
;;


let rec table2 i j = 
  if i > j then Prob.one_minus (table2 j i) else
    match (i, j) with 
    | 2, 3 | 2, 4 | 3, 4 | 4, 5 | 7, 8 -> Prob.half 
    | 1, _ | 2 , 7 | 2, 8 -> Prob.q2
    | _, _ -> Prob.q
;;
  

let rec table3 i j = 
  if i > j then Prob.one_minus (table2 j i) else
    match (i, j) with 
    | 2, 3 | 2, 4 | 3, 4 | 4, 5 | 7, 8 -> Prob.half 
    | 1, _ | 2 , 7 | 2, 8 -> Prob.q2
    | _, _ -> Prob.q
;;
  
   *)