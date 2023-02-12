module App = Eliom_registration.App (
  struct
    let application_name = "zz"
    let global_data_path = None
  end
);;

let %shared _ = Random.self_init ();;

Lwt_engine.set (new Lwt_engine.libev ()) ;;


[%%client
    let server = ~%(Eliom_client.server_function [%json: Json.u] (Comms.wrap Comms.Functions.server));;

    let () = Eliom_client.onload @@ Comms.bundle
        ~server
        ~client: Comms.Functions.client
        ~init: Comms.Functions.init
];;

App.create
    ~path: (Eliom_service.Path [])
    ~meth: (Eliom_service.Get Eliom_parameter.unit)
    (fun (str) () -> 
        Lwt.return Eliom_content.Html.D.(html
            (head (title (txt "Tourney Tracker")) [])
            (body [])
        )
    )
    ;;

;;print_endline "here";;





            
            (*log (
            Json.u_of_t @@ Data.yojson_of_t (Simulator.sim_scheme
                ~iters:10
                ~luck:1.
                (Scheme.Format ((module Round_robin), (33, 1)))
            )*)
(*
open%shared Lwt.Infix
(*
let%shared ( >>= ) l f = Lwt.bind l f;;
let%shared ( >>> ) l f = Lwt.bind l (fun x -> Lwt.return f x);;*)


let%shared get_str_list ~scheme ~iters ~luck () =
    let data = Simulator.sim_scheme ~luck ~iters scheme in
    let lst = [
    "DECAY" ^ Math.to_pct ~digits:2 data.decay;
    "Hyperparameters: iters = " ^ string_of_int iters ^ ", luck = " ^ string_of_float luck;
    "Format: " ^ Scheme.name scheme ^ ".";
    "Decay: " ^ Math.to_pct ~digits:2 data.decay ^ " (" ^ Math.to_pct ~digits:2 data.margin ^ ")" ;
    "Imbalance: " ^ Math.to_pct ~digits:2 (Data.calculate_imbalance data false) ^ " (" ^ Math.to_pct ~digits:2 (Stats.binom_error_formula ~iters ~cats:(Scheme.number_of_teams scheme)) ^ ")"
    ] in lst
;;

let%client init_client ~scheme ~iters ~luck () =
    Eliom_client.change_page_uri "bracket"
    >|= get_str_list ~scheme ~iters ~luck
    >|= List.hd
    >|= (^) (Scheme.kind_t scheme ^ "?str=")
    >>= Eliom_client.change_page_uri
;;

let builder : type a b . (a, b) Param_specs.Scheme.s -> unit = fun x ->
    let param = Param_specs.((Int_def ("iters", 10000) ** Float_def ("luck", 1.)) ** (Param_specs.Scheme.as_specs x)) in
    let _ = App.create
        ~path: (Eliom_service.Path [Scheme.kind_s (Param_specs.Scheme.as_scheme x)])
        ~meth: (Eliom_service.Get (Eliom_parameter.string "str"))
        (fun (str) () -> 
            Lwt.return Eliom_content.Html.D.(html
                (head (title (txt "Tourney Tracker")) [])
                (body [p [txt str]])
            )
        )
    
    in 
    let calculating_service = App.create
    ~path:(Eliom_service.Path [Scheme.kind_s (Param_specs.Scheme.as_scheme x) ^ "_calculating"])
    ~meth:(Eliom_service.Get (Param_specs.get_eliom_param param))
    (fun ((iters, luck), specs) () ->
        let scheme = Scheme.Format ((Param_specs.Scheme.as_scheme x), specs) in
        
         let t, u = Lwt.wait in
         let _ = [%client (init_client ~%scheme ~%iters ~%luck t () : unit Lwt.t) ] in
         let r = Lwt.return Eliom_content.Html.D.(html


         (head (title (txt "Tourney Tracker")) [])
         (body [p [txt "Calculating..."]])
     )
     in
     r
    )


    in let _ = App.create
        ~path:(Eliom_service.Path [Scheme.kind_s (Param_specs.Scheme.as_scheme x)])
        ~meth:(Eliom_service.Get Eliom_parameter.unit)
        (fun () () ->
            let f = Eliom_content.Html.D.Form.get_form ~service:calculating_service (Param_specs.get_form_function param) in
            Lwt.return @@
            Eliom_content.Html.D.(html
                (head (title (txt "")) [])
                (body [f])))
    

    in let internal_service = App.create
        ~path:(Eliom_service.Path [Scheme.kind_s (Param_specs.Scheme.as_scheme x) ^ "_internal"])
        ~meth:(Eliom_service.Get (Param_specs.get_eliom_param param))
        (fun ((iters, luck), specs) () ->
            let scheme = Scheme.Format ((Param_specs.Scheme.as_scheme x), specs) in
            let lst = get_str_list ~scheme ~iters ~luck () in
            Lwt.return Eliom_content.Html.D.(html
                (head (title (txt "Tourney Tracker")) [])
                (body [p [txt (List.hd lst)]])
            )
        )

    in let _ = App.create
        ~path:(Eliom_service.Path [Scheme.kind_s (Param_specs.Scheme.as_scheme x) ^ "_pre"])
        ~meth:(Eliom_service.Get Eliom_parameter.unit)
        (fun () () ->
            let f = Eliom_content.Html.D.Form.get_form ~service:internal_service (Param_specs.get_form_function param) in
            Lwt.return @@
            Eliom_content.Html.D.(html
                (head (title (txt "")) [])
                (body [f])))
    in ()

;;


let () =
    builder Param_specs.Scheme.Round_robin;
    builder Param_specs.Scheme.Bracket;
;;




let%server log str = Lwt_io.write_line Lwt_io.stdout str;;

let%client log =
  ~%(Eliom_client.server_function [%json: string] log);;

let%client () =
  Eliom_client.onload
    (* NB The service underlying the server_function isn't available
       on the client before loading the page. *)
    (fun () ->
       Lwt.async
         (fun () -> log "Hello from the client to the server!"))
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
*)*)