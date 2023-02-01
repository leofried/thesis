module App = Eliom_registration.App (
  struct
    let application_name = "zz"
    let global_data_path = None
  end
);;
(*
type%shared messages =
    Data.t
    [@@deriving json]

let%server bus = Eliom_bus.create [%json: messages]



let%server count = ref 0
let%server () = Lwt.async (fun () -> 
       	Lwt_stream.iter (fun () -> incr count) (Eliom_bus.stream bus))
*)


let%client init_client ~scheme_builder ~specs ~iters ~luck () =
    Eliom_lib.alert "Hello!";
    let scheme = Scheme.Format (scheme_builder, specs) in
    let data = Simulator.sim_scheme ~luck ~iters scheme in
    let lst = [
      "DECAY" ^ Math.to_pct ~digits:2 data.decay;
      "Hyperparameters: iters = " ^ string_of_int iters ^ ", luck = " ^ string_of_float luck;
      "Format: " ^ Scheme.name scheme ^ ".";
      "Decay: " ^ Math.to_pct ~digits:2 data.decay ^ " (" ^ Math.to_pct ~digits:2 data.margin ^ ")" ;
      "Imbalance: " ^ Math.to_pct ~digits:2 (Data.calculate_imbalance data false) ^ " (" ^ Math.to_pct ~digits:2 (Stats.binom_error_formula ~iters ~cats:(Scheme.number_of_teams scheme)) ^ ")"
    ] in
    (Eliom_client.change_page_uri (Scheme.kind scheme_builder ^ "?str=" ^ (List.hd lst)));
;;

let builder (scheme_builder : (_, _) Scheme.s) =
    let param = Param_specs.((Int_def ("iters", 10000) ** Float_def ("luck", 1.)) ** Scheme.params scheme_builder) in
    let _ = App.create
        ~path: (Eliom_service.Path [Scheme.kind scheme_builder])
        ~meth: (Eliom_service.Get (Eliom_parameter.string "str"))
        (fun (str) () -> 
            Lwt.return Eliom_content.Html.D.(html
                (head (title (txt "Tourney Tracker")) [])
                (body [p [txt str]])
            )
        )
    
    in let calculating_service = App.create
        ~path:(Eliom_service.Path [Scheme.kind scheme_builder ^ "_calculating"])
        ~meth:(Eliom_service.Get (Param_specs.get_eliom_param param))
        (fun ((iters, luck), specs) () ->
            let _ = [%client (init_client ~%scheme_builder ~%specs ~%iters ~%luck () : unit Lwt.t) ] in
            Lwt.return Eliom_content.Html.D.(html
                (head (title (txt "Tourney Tracker")) [])
                (body [p [txt "Calculating"]])
            )
        )


    in let _ = App.create
        ~path:(Eliom_service.Path [Scheme.kind scheme_builder])
        ~meth:(Eliom_service.Get Eliom_parameter.unit)
        (fun () () ->
            let f = Eliom_content.Html.D.Form.get_form ~service:calculating_service (Param_specs.get_form_function param) in
            Lwt.return @@
            Eliom_content.Html.D.(html
                (head (title (txt "")) [])
                (body [f])))
    in ()
;;


let () =
    builder Scheme.Round_robin;
(*    builder Bracket.eliom; *)
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
             (body [f])))



let round_robin_service = builder
    ~name: "round_robin"
    ~param: Param_specs.((Int "number_of_teams") ** (Int_def ("cycles", 1)))
    ~make: (fun (number_of_teams, cycles : int * int) -> Round_robin.make 
        ~number_of_teams
        ~cycles
    )
;;
*)
print_endline " here";;