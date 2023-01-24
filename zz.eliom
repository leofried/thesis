
let f ~(luck : float) ~(iters : int) (scheme : Scheme.t) =
    let data = Simulator.sim_scheme ~luck ~iters scheme in
    [
      "Hyperparameters: iters = " ^ string_of_int iters ^ ", luck = " ^ string_of_float luck;
      "Format: " ^ scheme.Scheme.name ^ ".";
      "Decay: " ^ Math.to_pct ~digits:2 data.decay ^ " (" ^ Math.to_pct ~digits:2 data.margin ^ ")" ;
      "Imbalance: " ^ Math.to_pct ~digits:2 (Data.calculate_imbalance data false) ^ " (" ^ Math.to_pct ~digits:2 (Stats.binom_error_formula ~iters ~cats:scheme.number_of_teams) ^ ")"
    ]
;;

let builder ~name ~param ~make =
    let param_ = Param_specs.((Int_def ("iters", 10000) ** Float_def ("luck", 1.)) ** param) in
    let results_service = Eliom_registration.Html.create
        ~path: (Eliom_service.Path [name])
        ~meth: (Eliom_service.Get Eliom_parameter.(Param_specs.get_eliom_param param_))
        (fun ((iters, luck), specs) () -> 
            let scheme = make specs in
            let data = f ~iters ~luck scheme in
            Lwt.return Eliom_content.Html.D.(html
                (head (title (txt "Tourney Tracker")) [])
                (body (List.map (fun str -> p [txt str]) data))
            )
        )
    in let _ = Eliom_registration.Html.create
        ~path:(Eliom_service.Path [name])
        ~meth:(Eliom_service.Get Eliom_parameter.unit)
        (fun () () ->
            let f = Eliom_content.Html.D.Form.get_form ~service:results_service (Param_specs.get_form_function param_) in
            Lwt.return @@
            Eliom_content.Html.D.(html
                (head (title (txt "")) [])
                (body [f])))
    in ()
;;


(*
let bracket_service = service_builder
    ~path: ["bracket"]
    ~params: Eliom_parameter.(string "bracket")
    ~f: (fun (bracket) -> Ocaml.Bracket.make
        ~bracket: (Ocaml.Bracket.string_to_bracket bracket)
    )
;;

let pool_play_service = service_builder
    ~path: ["pool_play"]
    ~params: Eliom_parameter.(int "number_of_teams" ** int "pool_count" ** string "bracket")
    ~f: (fun (number_of_teams, (pool_count, bracket)) -> Ocaml.Pool_play.make
        ~number_of_teams
        ~pool_count
        ~bracket: (Ocaml.Bracket.string_to_bracket bracket)
    )
;;




let s = 
    Eliom_registration.Html.create
        ~path: (Eliom_service.Path [])
        ~meth: (Eliom_service.Get Eliom_parameter.(opt (int "test") ** int "zzz"))
        (fun (test, zzz) () -> 
            Lwt.return Eliom_content.Html.D.(html
                (head (title (txt "Tourney Tracker")) [])
                (body (List.map (fun str -> p [txt str]) [match test with | None -> "none" | Some x -> string_of_int x]))
            );
        )

let create_form =
    (fun (number_name, n) ->
     Eliom_content.Html.D.(
        [p [txt "Write an int: ";
          Form.input ~input_type:`Text ~name:number_name ~value:7 Form.int;
          txt "Write an int: ";
          br();
          Form.input ~input_type:`Text ~name:n ~value:34 Form.int;
          Form.input ~input_type:`Submit ~value:"Click" Form.string]]
      ))
  
  let form =
    Eliom_registration.Html.create
      ~path:(Eliom_service.Path ["form"])
      ~meth:(Eliom_service.Get Eliom_parameter.unit)
      (fun () () ->
         let f = Eliom_content.Html.D.Form.get_form ~service:s create_form in
         Lwt.return @@
         Eliom_content.Html.D.(html
             (head (title (txt "")) [])
             (body [f])))*)



let round_robin_service = builder
    ~name: "round_robin"
    ~param: Param_specs.((Int "number_of_teams") ** (Int_def ("cycles", 1)))
    ~make: (fun (number_of_teams, cycles : int * int) -> Round_robin.make 
        ~number_of_teams
        ~cycles
    )
;;

print_endline " here";;