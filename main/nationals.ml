open Util;;


type team = string;;
type pool =
  | Five of team list
  | Four of (team * team * team)
;;



type game = 
  | Five of team * team
  | Four of team
  | FourNone
;;

type schedule = game list;;
type conflict = (team * team, unit) Hashtbl.t;;

let schedule_to_string = Lists.to_string (function | Five (a, b) -> Tuple.to_string Fun.id Fun.id (a, b) | Four a -> a | FourNone -> "BYE");;



let get_all_five_schedules pool : schedule list =
  let a = List.nth pool 0 in
  let b = List.nth pool 1 in
  let e = List.nth pool 4 in

  let get_pool_games pool =
    let rec f t lst = function
      | [] -> lst
      | hd :: tl -> f t ((t, hd) :: lst) tl
    in
    let rec g lst = function
      | [] -> lst
      | hd :: tl -> g (f hd lst tl) tl
    in
    g [] pool
  in

  let check_duplicates ((t1, t2)) ((t3, t4)) =
    t1 <> t3 && t1 <> t4 && t2 <> t3 && t2 <> t4
  in

  let check_next_game (schedule) (g) =
    match schedule with
    | [] -> true
    | hd :: _ -> check_duplicates hd g
  in

  let rec add_next_game (schedule, to_go)  =
    match to_go with
    | [] -> []
    | hd :: tl ->
      (if check_next_game schedule hd then [hd :: schedule, tl] else []) @
      List.map (Tuple.map_right (List.cons hd)) (add_next_game (schedule, tl))
  in

  let rec f (schedule, to_go) =
    match to_go with
    | [] -> [schedule]
    | _ -> List.flatten (List.map f (add_next_game (schedule, to_go)))
  in

  f ([], get_pool_games pool)
  |> List.filter (fun (l : (team * team) list) -> let x, y = List.hd l in let w, z = List.nth l 9 in ((x = e || y = e) && (w = e || z = e)))
  |> List.filter (fun (l : (team * team) list) -> let x, y = List.nth l 1 in let w, z = List.nth l 2 in not (x = a && y = b) && not (w = a && z = b))
  |> List.map (List.map (fun (a, b) -> Five (a, b)))
;;

let get_all_four_schedules (a, b, c) = 
 let f (a, b, c) = [Four a; Four b; Four c; Four a; Four b; Four c; Four a; Four b; Four c; FourNone] in
  let g (a, b, c) = [Four a; Four b; Four a; Four c; Four b; Four a; Four c; Four b; Four c; FourNone] in
 let h (a, b) = [Four a; FourNone; Four a; Four b; Four a; Four b; FourNone; Four b; FourNone;] in
 let _ = f(a, b, c) @ g (a, b, c) @ h (a, b) in 
  [(*FourNone :: (h (a,b)); FourNone :: (h (b,a)); (h (a,b)) @ [FourNone]; h(b, a) @ [FourNone];  *)
  f (a, b, c); f (a, c, b); f (b, a, c); f (b, c, a); f (c, a, b); f (c, b, a);
  g (a, b, c); g (a, c, b); g (b, a, c); g (b, c, a); g (c, a, b); g (c, b, a);
]
;;


let rec count_conflicts (conflicts) (college : schedule) (club : schedule)  =
  match college, club with
  | [], [] -> 0
  | Five (c1, c2) :: c, Five (cl1, cl2) :: cl -> 
    (if 
      Hashtbl.mem conflicts (c1, cl1) then Hashtbl.find conflicts (c1, cl1)
    else if 
      Hashtbl.mem conflicts (c1, cl2)then Hashtbl.find conflicts (c1, cl2) 
    else if 
      Hashtbl.mem conflicts (c2, cl1)then Hashtbl.find conflicts (c2, cl1) 
    else if 
      Hashtbl.mem conflicts (c2, cl2)then Hashtbl.find conflicts (c2, cl2) 
    else if 
      Hashtbl.mem conflicts (cl1, c1)then Hashtbl.find conflicts (cl1, c1) 
    else if 
      Hashtbl.mem conflicts (cl1, c2)then Hashtbl.find conflicts (cl1, c2) 
    else if 
      Hashtbl.mem conflicts (cl2, c1)then Hashtbl.find conflicts (cl2, c1) 
    else if 
      Hashtbl.mem conflicts (cl2, c2)then Hashtbl.find conflicts (cl2, c2)
    else
      0)
    + count_conflicts conflicts c cl
  | Four four :: t1 , Five (c1, c2) :: t2 | Five (c1, c2) :: t2, Four four :: t1 ->
    (if 
      Hashtbl.mem conflicts (four, c1) then Hashtbl.find conflicts (four, c1) 
    else if 
      Hashtbl.mem conflicts (four, c2) then Hashtbl.find conflicts (four, c2)
    else 0) +
      count_conflicts conflicts t1 t2
  | FourNone :: t1, _ :: t2 | _ :: t1, FourNone :: t2 -> count_conflicts conflicts t1 t2
  | _ -> assert false
;;

let filter_conflicts (s1 : (schedule list * int) list) (s2 : schedule list) conflicts cap =
  s1
  |> List.map (fun (lst, i) -> List.map (fun s -> s :: lst, i) s2)
  |> List.flatten
  |> List.map  (fun (lst, i) -> lst, List.fold_left (fun i l -> i + let h = List.hd lst in count_conflicts conflicts h l) i (List.tl lst)) 
  |> List.filter (fun (_, i) -> i <= cap)
  |> Debug.hold (fun lst -> print_endline @@ string_of_int @@ List.length lst)
;;

let get_all_full_schedules conflicts cap =
  List.fold_left (fun lst p -> filter_conflicts lst (match (p : pool) with | Five p -> get_all_five_schedules p | Four p -> get_all_four_schedules p) conflicts cap) [[], 0] 
;;