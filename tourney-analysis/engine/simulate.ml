open! Util
open! Std
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
  if len == 1 then ()  else
    simulate_schemes ~schemes:to_sim ~metric ~iters:(iters / len)
    |> Data.write ~metric;
  to_sim
;;

let rec best_simulate ~schemes ~metric ~prize ~iters ~cutoff  =
  let best = smart_simulate ~schemes ~metric ~prize ~iters ~cutoff in
  if List.length best = 1 then
    let scheme = List.hd best in
    print_endline @@ 
      string_of_int (Scheme.number_of_teams scheme) ^
      " teams with prize structue [" ^
      (Sexp.to_string (sexp_of_list sexp_of_int prize)) ^ 
      "]: " ^ (Sexp.to_string (Scheme.sexp_of_t scheme))
  else begin
    Data.print ~schemes:best ~metric ~prize ();
    best_simulate ~schemes ~metric ~prize ~iters ~cutoff
  end
;;