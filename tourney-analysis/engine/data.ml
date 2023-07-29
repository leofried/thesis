open! Util
open! Std
open! Schemes
open! Metrics

type t = (Scheme.t * Metric.t) list [@@deriving sexp];;

let get_file_name ~metric =
  let path = "analysis/" ^ Metric.kind metric in
  if not (Sys.file_exists path) then Sys.mkdir path 0;

  path ^ "/" ^
  string_of_int metric.specs.number_of_teams ^ "teams_" ^
  string_of_float metric.specs.luck ^ "luck_" ^
  string_of_float metric.specs.fidel ^ "fidel.sexp"
;;

let read ~metric  =
  let file_name = get_file_name ~metric in
  if Sys.file_exists file_name then 
    t_of_sexp (Sexp.read file_name)
  else
    []
;;

let write ~metric t =
  read ~metric
  |> List.collapse (Metric.combine metric) t
  |> sexp_of_t
  |> Sexp.write (get_file_name ~metric)
;;

let print ~metric : unit =
  List.iter
    (fun (scheme, data) ->
      let score, error, samples = Metric.score metric data in
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
      read ~metric
      |> List.sort_by_rev (Pair.right >> Metric.score metric >> (fun (a, _, _) -> a)) Float.compare
    )
;;