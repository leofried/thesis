open! Util
open! Engine
open! Schemes
open! Num


;;
(* 
Pools.get_all ~respectfulness:Strongly ~triviality:Efficient ~max_games:8 8 [1;1]
|> List.length
|> print_int *)


(* Prog.specific ();; *)

(* Progs.Prop_ord.f1 ~tiers:[[1];[2];[3;4];[5;6]] ~bracket:[4;2;0;0];; *)
(* Progs.Prop_ord.f1 ~tiers:[[1;2;3;4];[5;6;7;8]] ~bracket:[8;0;0;0];; *)



(*
let (specs : Specs.t) = Specs.{
  number_of_teams = 3;
  fidel = 0.;
  luck = 1.;
  distr = Random.(Floored (0., gaussian));
  tiebreaker = WorstCase;
};;

 let () =
      let scheme = Scheme.create (module Round_robin) 3 in
      let teams, play = Team.preconstruct specs in
      let f = play () in
      let _ = f in
      (Scheme.run scheme (play ()) teams)
      |> List.iter (fun k -> print_endline @@ string_of_int k.Team.id ^ ": " ^ string_of_float k.Team.skill)
 *)













  (* for n = 1 to 20 do
    for p = 2 to n do
      print_endline (string_of_int n ^ ", " ^ string_of_int p ^ ", " ^ string_of_int
      Schemes.((*Debug.time*) (fun () -> List.length (
        let prizes = [1; p] in 
        Pools.get_all ~respectfulness:Strongly ~triviality:Efficient ~max_games:8 n prizes
        |> List.filter (fun f -> List.for_all (fun m -> Metrics.Faithfulness.evaluate_format f m) prizes)
        (* |> List.map (fun x -> x |> Pools.sexp_of_t |> Sexp.to_string |> print_endline)  *)
      )) ()));
      ()
    done

  done *)


 (* let () =
  Schemes.(Debug.time (fun () -> print_endline @@ string_of_int @@ List.length (
    Multibracket.get_all ~max_games:3 ~respectfulness:Weakly ~triviality:Efficient (List.map Tier.new_tier [2;2;2;2;2;2]) [1;3;7]
  )) ());

  Schemes.(Debug.time (fun () -> print_endline @@ string_of_int @@ List.length (
    Multibracket.get_all ~max_games:5 ~respectfulness:Weakly ~triviality:Efficient (List.map Tier.new_tier [3;3;3;3]) [1;3;7]
  )) ());

  Schemes.(Debug.time (fun () -> print_endline @@ string_of_int @@ List.length (
    Multibracket.get_all ~max_games:6 ~respectfulness:Weakly ~triviality:Efficient (List.map Tier.new_tier [4;4;4]) [1;3;7]
  )) ());

  Schemes.(Debug.time (fun () -> print_endline @@ string_of_int @@ List.length (
    Multibracket.get_all ~max_games:7 ~respectfulness:Weakly ~triviality:Efficient (List.map Tier.new_tier [6;6]) [1;3;7]
  )) ());
;; *)

(* let number_of_teams = 10;;

let specs : Specs.t = Specs.{
  number_of_teams;
  fidel = 0.;
  luck = 1.;
  distr = Random.(Floored (0., gaussian));
};;

let games = 1000000
let teams, play = Debug.time (Team.preconstruct ~games) specs;;


let () =

Debug.time (fun () ->
  for i = 1 to number_of_teams do
    for j = (i+1) to number_of_teams do
      for _ = 1 to games do
        ignore (
          Team.play_game specs
            (List.nth_one teams i)
            (List.nth_one teams j)
        )
      done
    done
  done
) ();


Debug.time (fun () ->
  for i = 1 to number_of_teams do
    for j = (i+1) to number_of_teams do
      for _ = 1 to games do
        ignore (
          Team.play_game specs
            (List.nth_one teams i)
            (List.nth_one teams j)
        )
      done
    done
  done
) ();

() *)