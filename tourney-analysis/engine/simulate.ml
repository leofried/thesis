open! Util
open! Schemes

let simulate_schemes ~schemes ~metric ~iters =
  iters
  |> List.create
  |> List.map (fun i -> Metric.generate metric schemes, i)
  |> List.fold_left (fun xs (ys, _) -> List.combine xs ys |> List.map (Pair.uncurry (Metric.combine metric))) (List.map (fun _ -> Metric.empty metric) schemes)
  |> List.combine schemes
;;


let smart_simulate ~schemes ~metric ~prize ~iters ~cutoff =
  let data = Data.read ~schemes ~metric () in
  let best =
    data
    |> List.map Pair.right
    |> List.map (Metric.score metric prize)
    |> List.filter_map (fun (u, _, _) -> if Float.is_nan u then None else Some u)
    |> List.sort Float.compare
    |> (fun l -> if l = [] then [0.] else l)
    |> List.hd
  in
  let to_sim =
    data
    |> List.filter (fun (_, datum) ->
      let u, s, i = Metric.score metric prize datum in
      i = 0 || (u -. best) /. s < cutoff
    )
    |> List.map Pair.left
  in
  let len = List.length to_sim in
  print_endline (string_of_int len);
  simulate_schemes ~schemes:to_sim ~metric ~iters:(iters / len)
  |> Data.write ~metric
;;




(* let simulate_smart metric schemes = ();; *)




(*
let run_one_sim ~(scheme : Scheme.t) ~(metric : Metric.t) ~(luck : float) ~(fidel : float)
  let teams = Team.create ~fidel ~n:(Scheme.number_of_team scheme) in
  let results = Scheme.run scheme 



end



*)

(*SMART SIM TEMPERATURE, LOTS OF OPTIONALITY HERE

module M (Scheme : S.SCHEME) = struct

module Data = Data.M (Scheme);;

let run_one_sim (specs : Specs.t) (scheme : Scheme.t) : float =
  let teams = Team.make_n specs.fidel specs.number_of_teams  in

  let results = Scheme.run scheme teams in

  let real =
    results
    |> List.map (fun t -> t.Team.skill)
    |> Lists.top_of_list specs.number_advance
    |> Tuple.left
    |> Lists.fold (+.)
  in

  let best =
    teams
    |> List.map (fun t -> t.Team.skill)
    |> List.sort (Fun.flip compare)
    |> Lists.top_of_list specs.number_advance
    |> Tuple.left
    |> Lists.fold (+.)
  in

  best -. real
;;

let simulate_scheme (specs : Specs.t) (iters : int) (scheme : Scheme.t) : Stats.t =
  Team.set_luck specs.luck;
  Stats.of_list (List.init iters (fun _ -> run_one_sim specs scheme))
;;

let simulate_schemes (specs : Specs.t) (iters : int) (schemes : Scheme.t list) : unit =
  schemes
  |> List.map (fun scheme -> scheme, simulate_scheme specs iters scheme)
  |> Data.write specs
;;

let simulate_smart (specs : Specs.t) (iters : int) (data : Data.t list) : Data.t list =
  let best =
    data
    |> List.map (fun (_, stats) -> Stats.mean stats)
    |> Lists.fold min
  in
  let shares =
    data
    |> List.map (Tuple.map_right (fun stats -> (Stats.mean stats -. best) /. Stats.stderr stats))
    |> List.map (Tuple.map_right (fun i -> 1. /. (2. ** i)))
  in
  let total =
    shares
    |> List.map Tuple.right
    |> Lists.fold (+.)
  in
  shares
  |> List.map (Tuple.map_right (fun i -> int_of_float (i /. total *. (float_of_int iters))))
  |> List.map (fun (scheme, i) -> (scheme, simulate_scheme specs i scheme))
;;

let simulate_smart_looped ?(schemes : Scheme.t list = []) (specs : Specs.t) (iters : int) =
  simulate_schemes specs 1000 schemes;
  
  Debug.loop (fun () ->
    print_endline "cycle complete";
    Data.read specs
    |> simulate_smart specs iters
    |> Data.write specs
  )
;;
end*)