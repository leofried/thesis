let build_page ?(iters = 10000) ?(luck = 1.) scheme =
    Lwt.return Eliom_content.Html.D.(html (head (title (txt "Tourney Tracker")) []) (body (List.map (fun str -> p [txt str]) (Ocaml.f ~iters ~luck scheme))));;
;;

let service_builder params f =
    ignore @@ Eliom_registration.Html.create
        ~path:(Eliom_service.Path ["round_robin"])
        ~meth:(Eliom_service.Get Eliom_parameter.((int "iters" ** float "luck") ** params))
        (fun ((iters, luck), other) () -> build_page ~iters ~luck (f other));
    ignore @@ Eliom_registration.Html.create
        ~path:(Eliom_service.Path ["round_robin"])
        ~meth:(Eliom_service.Get Eliom_parameter.(int "iters" ** params))
        (fun (iters, other) () -> build_page ~iters (f other));
    ignore @@ Eliom_registration.Html.create
        ~path:(Eliom_service.Path ["round_robin"])
        ~meth:(Eliom_service.Get Eliom_parameter.(float "luck" ** params))
        (fun (luck, other) () -> build_page ~luck (f other));
    ignore @@ Eliom_registration.Html.create
        ~path:(Eliom_service.Path ["round_robin"])
        ~meth:(Eliom_service.Get Eliom_parameter.(params))
        (fun (other) () -> build_page (f other))
;;

service_builder
    Eliom_parameter.(int "number_of_teams" ** int "cycles")
    (fun (number_of_teams, cycles) -> Ocaml.Round_robin.make ~number_of_teams ~cycles)