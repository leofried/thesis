let four_team_top_three (teams : Bye.t list) : Bye.t list =
  match teams with
  | [t1; t2; t3; t4] ->
    let w1, l1 = Bye.play_game t1 t4 in
    let w2, l2 = Bye.play_game t2 t3 in
    let ww, wl = Bye.play_game w1 w2 in
    let lw, ll = Bye.play_game l1 l2 in
    [ww; wl; lw; ll]
  | _ -> Util.error ()
;;

let rec partition = function
 | 0 | 3 -> true
 | 1 | 2 -> false
 | n -> partition (n - 4)
;;

let rec n_team_top_three (teams : Bye.t list) : Bye.t list = 
  match List.length teams with
  | 0 | 1 | 2 | 3 -> Util.error ()
  | 4 -> four_team_top_three teams
  | _ ->
    let left = n_team_top_three @@ List.filteri (fun i _ -> partition i) teams in
    let right = n_team_top_three @@ List.filteri (fun i _ -> Bool.not (partition i)) teams in
    match left, right with
    | l1 :: l2 :: l3 :: ls, r1 :: r2 :: r3 :: rs ->
      let w, l = Bye.play_game l1 r1 in
      let s1w, s1l = Bye.play_game l2 r3 in
      let s2w, s2l = Bye.play_game r2 l3 in
      let fw, fl = Bye.play_game s1w s2w in
      w :: l :: fw :: fl :: s1l :: s2l :: ls @ rs
    | _ -> Util.error ()
;;

let n_team_top_one (recharge : bool) (teams : Bye.t list) : Bye.t list =
  match n_team_top_three teams with
  | t1 :: t2 :: t3 :: ts ->
    let sw, sl = Bye.play_game t2 t3 in
    let fw, fl = Bye.play_game t1 sw in
    let rw, rl = 
      if recharge && fw = sw then
        Bye.play_game t1 sw
      else
        fw, fl
    in
    rw :: rl :: sl :: ts
  | _ -> Util.error ()
;;
    
let run_bracket (recharge : bool) (teams : Team.t list) : Team.t list = 
  teams
  |> Bye.fill
  |> n_team_top_one recharge
  |> Bye.empty
;;

let make (recharge : bool) (number_of_teams : int) : Scheme.t =
  Scheme.make_scheme ("Double Elimination (" ^ (if recharge then "" else "no ") ^ "recharge)") number_of_teams (run_bracket recharge)
;;