(*
let t1 = Team.make ~skill:0. ();;

let t2 = Team.make ~skill:0. ();;

let t3 = Team.make ~skill:0. ();;

let t4 = Team.make ~skill:0. ();;

let teams = t1 :: t2 :: t3 :: t4 :: []

let sb : Scheme.t = Scheme_simple_bracket.make 4;;
let lb : Scheme.t = Scheme_ladder_bracket.make 4;;

let rr : Scheme.t = Scheme_round_robin.make 4;;

for i = 0 to 500 do
  Util.set_seed (Array.make 1 i);
  print_endline @@ Team.to_string @@ List.hd @@ Scheme.run sb teams
done;;

print_endline "---------------";;

for i = 0 to 500 do
  Util.set_seed (Array.make 1 i);
  print_endline @@ Team.to_string @@ List.hd @@ Scheme.run lb teams
done;;

print_endline "---------------";;

for i = 0 to 500 do
  Util.set_seed i;
  print_endline @@ Team.to_string @@ List.hd @@ Scheme.run rr teams
done;;

List.iteri
  (fun i n -> print_endline ("If " ^ (Int.to_string i) ^ " teams are good, then " ^ (Int.to_string n) ^ " of the time, a good team wins.")) 
  (Old_analysis.analyze
  ~seed: 0
  ~luck: 1.
  ~bound: 2.
  ~good: 0.
  ~iter: 1000
  (Scheme_simple_bracket.make 20))
*)

Util.set_seed ();;


let n = 6;;
let iters = 1000000;;
let luck = 0.33;;
let recharge = true;;

let f s = Analysis.analyze_scheme (s n) ~iters ~luck;;

f (Scheme_round_robin.make);;
f (Scheme_pool_to_bracket.make ~pool_count:1 ~teams_to_bracket:1);;
f (Scheme_pool_to_bracket.make ~pool_count:1 ~teams_to_bracket:2);;
f (Scheme_pool_to_bracket.make ~pool_count:1 ~teams_to_bracket:3);;
f (Scheme_pool_to_bracket.make ~pool_count:1 ~teams_to_bracket:4);;
f (Scheme_pool_to_bracket.make ~pool_count:1 ~teams_to_bracket:5);;
f (Scheme_pool_to_bracket.make ~pool_count:1 ~teams_to_bracket:6);;

(*
f (Scheme_simple_bracket.make);;
f (Scheme_double_elimination.make true);;
f (Scheme_double_elimination.make false);;
print_endline "";;
f (Scheme_pool_to_bracket.make ~pool_count:3 ~teams_to_bracket:4);;
f (Scheme_pool_to_bracket.make ~pool_count:3 ~teams_to_bracket:3);;
f (Scheme_pool_to_bracket.make ~pool_count:3 ~teams_to_bracket:2);;
f (Scheme_pool_to_bracket.make ~pool_count:3 ~teams_to_bracket:1);;
print_endline "";;
f (Scheme_pool_to_bracket.make ~pool_count:4 ~teams_to_bracket:16);;
f (Scheme_pool_to_bracket.make ~pool_count:4 ~teams_to_bracket:12);;
f (Scheme_pool_to_bracket.make ~pool_count:4 ~teams_to_bracket:8);;
f (Scheme_pool_to_bracket.make ~pool_count:4 ~teams_to_bracket:4);;
print_endline "";;
f (Scheme_pool_to_bracket.make ~pool_count:5 ~teams_to_bracket:8);;
f (Scheme_pool_to_bracket.make ~pool_count:5 ~teams_to_bracket:10);;
f (Scheme_pool_to_bracket.make ~pool_count:5 ~teams_to_bracket:15);;
f (Scheme_pool_to_bracket.make ~pool_count:5 ~teams_to_bracket:16);;
print_endline "";;
f (Scheme_pool_to_bracket.make ~pool_count:6 ~teams_to_bracket:8);;
f (Scheme_pool_to_bracket.make ~pool_count:6 ~teams_to_bracket:12);;
f (Scheme_pool_to_bracket.make ~pool_count:6 ~teams_to_bracket:16);;
f (Scheme_pool_to_bracket.make ~pool_count:6 ~teams_to_bracket:18);;



f (Scheme_pool_to_bracket.make ~pool_count:2 ~teams_to_bracket:8);;
f (Scheme_pool_to_bracket.make ~pool_count:2 ~teams_to_bracket:6);;
f (Scheme_pool_to_bracket.make ~pool_count:2 ~teams_to_bracket:4);;
f (Scheme_pool_to_bracket.make ~pool_count:2 ~teams_to_bracket:2);;
f (Scheme_pool_to_bracket.make ~pool_count:3 ~teams_to_bracket:3);;
f (Scheme_pool_to_bracket.make ~pool_count:3 ~teams_to_bracket:4);;
f (Scheme_pool_to_bracket.make ~pool_count:3 ~teams_to_bracket:6);;
f (Scheme_pool_to_bracket.make ~pool_count:4 ~teams_to_bracket:4);;
f (Scheme_pool_to_bracket.make ~pool_count:4 ~teams_to_bracket:8);;
f (Scheme_pool_to_bracket.make ~pool_count:12 ~teams_to_bracket:12);;

Analysis.calculate_better_team_win_pct ~luck ~iters;;*)
