
(* Prog.run() *)

let () =
  Schemes.(Util.Debug.time (fun () -> print_endline @@ string_of_int @@ List.length (
    Pools.get_all ~respectfulness:Strongly ~triviality:Efficient ~max_games:8 12 [4]
    |> List.map (fun x -> x |> Pools.sexp_of_t |> Util.Sexp.to_string |> print_endline)
  )) ());

  (* for n = 1 to 20 do
    for p = 2 to n do
      print_endline (string_of_int n ^ ", " ^ string_of_int p ^ ", " ^ string_of_int
      Schemes.((*Util.Debug.time*) (fun () -> List.length (
        let prizes = [1; p] in
        Pools.get_all ~respectfulness:Strongly ~triviality:Efficient ~max_games:8 n prizes
        |> List.filter (fun f -> List.for_all (fun m -> Metrics.Faithfulness.evaluate_format f m) prizes)
        (* |> List.map (fun x -> x |> Pools.sexp_of_t |> Util.Sexp.to_string |> print_endline)  *)
      )) ()));
      ()
    done

  done *)


 (* let () =
  Schemes.(Util.Debug.time (fun () -> print_endline @@ string_of_int @@ List.length (
    Multibracket.get_all ~max_games:3 ~respectfulness:Weakly ~triviality:Efficient (List.map Tier.new_tier [2;2;2;2;2;2]) [1;3;7]
  )) ());

  Schemes.(Util.Debug.time (fun () -> print_endline @@ string_of_int @@ List.length (
    Multibracket.get_all ~max_games:5 ~respectfulness:Weakly ~triviality:Efficient (List.map Tier.new_tier [3;3;3;3]) [1;3;7]
  )) ());

  Schemes.(Util.Debug.time (fun () -> print_endline @@ string_of_int @@ List.length (
    Multibracket.get_all ~max_games:6 ~respectfulness:Weakly ~triviality:Efficient (List.map Tier.new_tier [4;4;4]) [1;3;7]
  )) ());

  Schemes.(Util.Debug.time (fun () -> print_endline @@ string_of_int @@ List.length (
    Multibracket.get_all ~max_games:7 ~respectfulness:Weakly ~triviality:Efficient (List.map Tier.new_tier [6;6]) [1;3;7]
  )) ());
;; *)

(* let number_of_teams = 10;;

let specs : Engine.Specs.t = Engine.Specs.{
  number_of_teams;
  fidel = 0.;
  luck = 1.;
  distr = Util.Random.(Floored (0., gaussian));
};;

let games = 1000000
let teams, play = Util.Debug.time (Engine.Team.preconstruct ~games) specs;;


let () =

Util.Debug.time (fun () ->
  for i = 1 to number_of_teams do
    for j = (i+1) to number_of_teams do
      for _ = 1 to games do
        ignore (
          Engine.Team.play_game specs
            (Util.List.nth_one teams i)
            (Util.List.nth_one teams j)
        )
      done
    done
  done
) ();


Util.Debug.time (fun () ->
  for i = 1 to number_of_teams do
    for j = (i+1) to number_of_teams do
      for _ = 1 to games do
        ignore (
          Engine.Team.play_game specs
            (Util.List.nth_one teams i)
            (Util.List.nth_one teams j)
        )
      done
    done
  done
) ();

() *)