
(* Prog.run() *)

for max_games = 0 to 10 do
  Schemes.(Util.Debug.time (fun () -> print_int @@ List.length (
    Multibracket.get_all ~max_games ~respectfulness:Weakly ~triviality:Efficient (List.map Tier.new_tier [2;2;2;2;2;2]) [1;2]
  )) ())
done;;

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