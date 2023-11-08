

type round = int * int * int * int
type level = College | Club

let all_rounds : round list =
  [
    (* 4, 0, 0, 0;
    0, 4, 0, 0;
    0, 0, 4, 0;
    0, 0, 0, 4; *)

    3, 1, 0, 0;
    3, 0, 1, 0;
    3, 0, 0, 1;
    1, 3, 0, 0;
    0, 3, 1, 0;
    0, 3, 0, 1;
    (* 1, 0, 3, 0;
    0, 1, 3, 0;
    0, 0, 3, 1;
    1, 0, 0, 3;
    0, 1, 0, 3;
    0, 0, 1, 3; *)

    2, 2, 0, 0;
    2, 0, 2, 0;
    2, 0, 0, 2;
    0, 2, 2, 0;
    0, 2, 0, 2;
    0, 0, 2, 2;

    2, 0, 1, 1;
    2, 1, 0, 1;
    2, 1, 1, 0;
    0, 2, 1, 1;
    1, 2, 0, 1;
    1, 2, 1, 0;
    0, 1, 2, 1;
    1, 0, 2, 1;
    1, 1, 2, 0;
    0, 1, 1, 2;
    1, 0, 1, 2;
    1, 1, 0, 2;
 
    1, 1, 1, 1;
  ];;

  type division = int list;;
  type all_divisions = int list * int list * int list * int list


  let rec is_ok (schedule : division) = function
    | College -> begin
      if List.fold_left (+) 0 schedule > 12 then false else
      match schedule with
      | [] -> true
      | x :: y :: _ when x + y >= 4 -> false
      | 2 :: 1 :: 2 :: _ -> false
      | 1 :: 2 :: 1 :: _  -> false
      | _ :: tl -> is_ok tl College end
    | Club ->
      if List.fold_left (+) 0 schedule > 6 then false else
      match schedule with
      | [] -> true
      | x :: y :: _ when x + y >= 3 -> false
      | 1 :: 1 :: 1 :: _ -> false
      | _ :: tl -> is_ok tl Club
    ;;

  let is_all_ok (s1, s2, s3, s4 : all_divisions) = is_ok s1 College && is_ok s2 College && is_ok s3 Club && is_ok s4 Club


  let recurse (schedules : all_divisions list) =
    schedules
    |> List.rev_map (fun (x1, x2, x3, x4) -> List.rev_map (fun (y1, y2, y3, y4) -> y1 :: x1, y2 :: x2, y3 :: x3, y4 :: x4) all_rounds)
    |> List.flatten
    |> List.filter is_all_ok
  ;;

  let rec solve (schedules : all_divisions list) (i : int) = 
    if i = 0 then schedules else solve (recurse schedules) (i - 1)
  ;;

  






(* 
type team = string
type game = team * team
type round = game list
type s0hedule = round list
type 0onflicts = (game, unit) Hashtbl.t (*include self, both dirs*)


let get_teams_playing_this_round (round : round) : team list =
  round |> List.rev_map (fun (a, b) -> [a; b]) |> List.flatten
;;

let legal_game_add (schedule : schedule) (conflicts : conflicts) ((t1, t2) : game) : bool =
  (
    schedule
    |> List.hd
    |> get_teams_playing_this_round
    |> List.for_all (fun team -> not (Hashtbl.mem conflicts (team, t1) || Hashtbl.mem conflicts (team, t2)))
  ) && (
    (
      match schedule with
      | [] -> assert false
      | [hd] -> hd
      | hd :: last :: _ -> hd @ last
    ) 
    |> (
      let rec dup_exist = function
        | [] -> false
        | hd::tl -> List.exists ((=) hd) tl || dup_exist tl
      in dup_exist
    )
    |> not
  )
;;

let add_game (schedule : schedule ) (game : game) (fields : int): schedule =
  if List.length (List.hd schedule) = fields then
    [game] :: schedule
  else
    (game :: List.hd schedule ) :: List.tl schedule
;;

let pop_all lst = 
  let rec f hd tl =
    match tl with
    | [] -> []
    | x :: tl ->
      (x, hd @ tl) :: (f (x :: hd) tl)
  in
  f [] lst
;;

let recurse (schedules : (schedule * game list) list) (conflicts : conflicts) (fields : int) :  (schedule * game list) list =
  schedules
  |> List.rev_map (fun (schedule, games) -> List.rev_map (fun (game, rest) -> schedule, game, rest) (pop_all games))
  |> List.flatten
  |> List.filter (fun (schedule, game, _) -> legal_game_add schedule conflicts game)
  |> List.rev_map (fun (schedule, game, rest) -> add_game schedule game fields, rest)
;;

let solve (games : game list) (conflicts : conflicts) (fields : int) : (schedule) list =
  let rec f (schedules : (schedule * game list) list) = 
    if List.length schedules = 0 || List.length (snd (List.hd schedules)) = 0 then
      schedules |> List.rev_map fst
    else
      f (recurse schedules conflicts fields)
  in 
  f [[[]], games]
;;

let rec make_pool (teams : team list) : game list =
  match teams with
  | [] -> []
  | hd :: tl ->
    List.rev_map (fun x -> hd, x) tl @ make_pool tl
;;

let pools = [["a"; "b"; "c"; "d"]; ["1"; "2"; "3"; "4"]];;
let teams = List.flatten pools;;
let games = List.flatten (List.rev_map make_pool pools);;

let conflicts = Hashtbl.create 10;;

solve games conflicts 2 *)