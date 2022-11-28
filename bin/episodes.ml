(*
let two () : unit =
  let iters = 1000000 in
  List.iter
    (fun luck -> 
      Analysis.analyze_scheme ~iters ~luck (Scheme_round_robin.make 3);
      Analysis.analyze_scheme ~iters ~luck (Scheme_simple_bracket.make 3);
    )
    [0.33; 1.00; 3.00]
  ;
  List.iter
    (fun n -> 
      List.iter
        (fun b -> 
          Analysis.analyze_scheme ~iters ~luck:1. (Scheme_pool_to_bracket.make ~pool_count:1 ~teams_to_bracket:(b+1) n);
        )
        (List.init n Fun.id)
      ; print_endline ""
    )
    (List.init 12 (fun x -> x + 1))
;;

*)

