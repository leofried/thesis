let build_page data = Lwt.return Eliom_content.Html.D.(html (head (title (txt "Tourney Tracker")) []) (body [p [txt data]]));;

Eliom_registration.Html.create
    ~path:(Eliom_service.Path ["pool_play"])
    ~meth:(Eliom_service.Get Eliom_parameter.(int "t" ** int "z"))
    (fun (t, z) () -> build_page (Help.data t z))
;;