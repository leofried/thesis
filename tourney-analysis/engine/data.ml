open! Util
open! Std
open! Schemes

type t = (Scheme.t * Metric.t) list [@@deriving sexp];;

let get_file_name ~metric =
  let path = "analysis/" ^ Metric.kind metric in
  if not (Sys.file_exists path) then Sys.mkdir path 0;

  path ^ "/" ^ Sexp.to_string (Specs.sexp_of_t metric.specs) ^ ".sexp"
;;

let read ?schemes ~metric () =
  let file_name = get_file_name ~metric in
  let raw = if Sys.file_exists file_name then 
    t_of_sexp (Sexp.read file_name)
  else
    []
  in match schemes with 
  | None -> raw
  | Some schemes ->
    List.map (fun scheme -> 
      scheme
      |> Fun.flip List.assoc_opt raw 
      |> Option.set (Metric.empty metric) 
      |> Pair.pair scheme
    ) schemes
;;

let write ~metric t =
  t
  |> List.append (read ~metric ())
  |> List.collapse (Metric.combine metric)
  |> sexp_of_t
  |> Sexp.write (get_file_name ~metric)
;;

let print ?schemes ~metric ~prize () : unit =
  let digits = 2 in
  (* let prizes = Prize.convert metric.Metric.specs.number_of_teams prize in *)
  List.iter
    (fun (scheme, data) ->
      let score, error, samples = Metric.score metric prize data in
        print_endline @@
        "" ^ 
        Math.to_pct ~digits score ^
        " [" ^
        Math.to_pct ~digits error ^
        "], on " ^
        Int.to_string samples ^
        " iters : " ^
        Sexp.to_string (Scheme.sexp_of_t scheme)
    )
    (
      read ?schemes ~metric ()
      |> 
      List.sort_by_rev (fun data ->
        data
        |> Pair.right 
        |> Metric.score metric prize
        |> (fun (a, _, _) -> a)
      ) Float.compare
    )
;;