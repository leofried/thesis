open! Util
open! Std
open! Schemes
open! Metrics
open! Engine

let general () = 

  for number_of_teams = 4 to 4 do      
    for max_games = 4 to 8 do
      print_string @@ "----" ^ string_of_int max_games;

      let metric : Metric.s = {
        metric = (module Decay : Metric.S);
        specs = {
          number_of_teams;
          fidel = 0.;
          luck = 1.;
          distr = Random.(Floored (0., gaussian));
          tiebreaker = WorstCase;
        };
      } in

      (* for i = 1 to (number_of_teams - 1) do
          let prize = [i] in *)
      List.iter (fun prize -> 

          let schemes = 
            Pools.get_all ~respectfulness:Strongly ~triviality:Efficient ~max_games ~allow_one:false number_of_teams prize
            |> List.map (Scheme.create (module Pools))
          in

          if List.length schemes |> Debug.print string_of_int = 0 then
            print_endline @@ string_of_int number_of_teams ^ "x" ^ string_of_int (List.fold_left (+) 0 prize) ^ " is empty."
          else
            Simulate.best_simulate ~schemes ~metric ~prize ~iters:10_000 ~cutoff:2.0

      ) [[1]; [2]; [3]; [1;2]; [1;3]; [2;3]; [1;2;3]]
    done
  done


let specific () =

  let number_of_teams = 6 in
  (* let max_games = 7 in *)
  let prize = [1;2] in
  
  let metric : Metric.s = {
    metric = (module Decay : Metric.S);
    specs = {
      number_of_teams;
      fidel = 0.;
      luck = 1.;
      distr = Random.Uniform (-1., 1.);
      (* Random.(Floored (0., gaussian)); think *)
      tiebreaker = WorstCase;
    };
  } in

  let schemes = 
    (* Pools.get_all ~respectfulness:Strongly ~triviality:Efficient ~max_games (*~allow_cycles:true*) number_of_teams prize
    (* |> (List.cons) Pools.{number_of_pools = 3; teams_per_pool = 1; cycles_per_pool = 1; multibracket = [[2;1;0];[1];[1]]}
    |> (List.cons) Pools.{number_of_pools = 3; teams_per_pool = 1; cycles_per_pool = 1; multibracket = [[2;1;0];[2;0];[1]]} *)
    (* |> List.filter (fun x  -> x.Pools.number_of_pools <> 1) *)
    |> List.filter (fun x  -> x.Pools.teams_per_pool <> 1)
    (* |> List.map (fun x  -> Pools.{x with cycles_per_pool = 0}) *)
    |> List.map (Scheme.create (module Pools)) *)
    [
      Scheme.create (module Mlq) Old;
      Scheme.create (module Mlq) New;
      (* Scheme.create (module Proper) [4;2;0;0] *)
    ]
  in

  (* let base : Pools.t = {number_of_pools = 2; teams_per_pool = 5; cycles_per_pool = 1; multibracket = []} in
  let schemes = 
    [
      {base with multibracket = [[4;2;0;0]; [4;2;0;3;0;0]]};
      {base with multibracket = [[4;2;0;0]; [  4;0;3;0;0]]};
      {base with multibracket = [[4;2;0;0]; [4;2;2;0;1;0]]};
      {base with multibracket = [[4;2;0;0]; [  4;2;0;1;0]]}; 
    ]
    |> List.map (Scheme.create (module Pools))
  in *)


  Debug.loop (fun () ->
    Data.print ~schemes ~metric ~prize ();
    Simulate.simulate_schemes ~metric ~schemes ~iters:10000 |> Data.write ~metric;
    (* Simulate.best_simulate ~schemes ~metric ~prize ~iters:10_000 ~cutoff:8.0; *)
    print_endline "---";
  )
;; 