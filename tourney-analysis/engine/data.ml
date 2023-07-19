open! Util;;
open! Std;;

type t = (Scheme.t * (Metric.v list)) list [@@deriving sexp];;

let get_file_name ~(specs : Specs.t) =
  "analysis/" ^
  string_of_int specs.number_of_teams ^ "teams_" ^
  string_of_float specs.luck ^ "luck_" ^
  string_of_float specs.fidel ^ "fidel.txt"
;;

let read ~specs =
  let file_name = get_file_name ~specs in
  if Sys.file_exists file_name then 
    t_of_sexp (Sexp.load_sexp file_name)
  else
    []
;;

let write ~specs data =
  read ~specs
  |> List.collapse (fun _ -> List.collapse Metric.combine) data
  |> sexp_of_t
  |> Sexp.write (get_file_name ~specs)
;;

let print ~specs (metric : Metric.t) : unit =
  List.iter
    (fun ((scheme, data)) ->
      let score, error, samples = Metric.score data in
        print_endline @@
        "" ^ 
        Math.to_pct ~digits:2 score ^
        " [" ^
        Math.to_pct ~digits:2 error ^
        "], on " ^
        Int.to_string samples ^
        " iters : " ^
        Sexp.to_string (Scheme.sexp_of_t scheme)
    )
    (
      read ~specs
      |> List.filter_map (fun (scheme, metrics) ->
        metric
        |> (Fun.flip List.assoc_opt) metrics
        |?> Pair.pair metric
        |?> Pair.pair scheme
      )
      |> List.sort_by_rev (Pair.right >> Metric.score >> (fun (a, _, _) -> a)) Float.compare
    )
;;