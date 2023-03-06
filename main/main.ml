open Util;;


Rand.set_seed () ;;
print_endline "" ;;




let number_teams = 23;;
let max_games = 8;;
let max_pools = 8;;
let base = 32;;

let k = 
  Partition.all_parts_cut number_teams max_pools
  |> List.map (fun p -> List.map (fun x -> Test.transform base (List.combine p x)) (Partition.all_parts_exact base (List.length p)))
  |> List.flatten
  |> List.filter (fun x -> Test.max_games x <= max_games)
  |> List.sort Test.compare
;;

List.iter (fun lst -> print_endline @@ Lists.to_string (fun (x, y) -> "(" ^ string_of_int x ^ ", " ^ string_of_float (Adic.to_float y) ^ ")") false lst) k;;

(*
let x =  [ (6, Adic.of_float 0.375); (3, Adic.of_float 0.15625); (3, Adic.of_float 0.15625); (3, Adic.of_float 0.15625); (3, Adic.of_float 0.15625)];;
print_endline @@ string_of_int (Test.max_games x);;


let x = [(6, 12); (3, 5); (3, 5); (3, 5); (3, 5)];;
let y = Test.transform 32 x;;
print_endline @@ string_of_bool (Test.validate y);;

print_endline @@ Lists.to_string (Lists.to_string string_of_int false) true (Partition.all_parts_cut 18 8);;*)
(*
let ff = [
  (6, 4);
  (5, 4);
  (5, 4);
  (5, 4);
]
(*[
  (5, 4);
  (4, 3);
  (4, 3);
  (4, 3);
  (4, 3);
];;*)

let f = List.map (Tuple.map_right (fun x -> Adic.of_float (Math.divide_int_int x base))) ff;;

print_endline @@ string_of_bool @@ Test.validate f;;
print_endline @@ string_of_int @@ Test.number_of_teams f;;
print_endline @@ string_of_int @@ Test.max_games f;;
print_endline @@ string_of_float @@ Test.min_equity f;;


*)
(*
let number_of_teams = 12;;
let number_advance = 4;;

let max_games = 8;;
let specs = {Data.default_specs with number_of_teams; number_advance; max_games};;



Data.print specs;;
Simulate.simulate_schemes specs 1000 (Sptb.build_all specs);;
Simulate.simulate_smart_looped specs 100_000;;

*)