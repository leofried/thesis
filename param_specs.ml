module P = Eliom_parameter;;


type (_, _) t = 
    | Int : string -> (int, [ `One of int] P.param_name) t
    | Int_def : string * int -> (int, [ `One of int] P.param_name) t
    | Float : string -> (float, [ `One of float] P.param_name) t
    | Float_def : string * float -> (float, [ `One of float] P.param_name) t
    | Prod : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
;;

let ( ** ) a b = Prod (a, b);;
          
let rec get_eliom_param : type a c. (a, c) t -> (a, [`WithoutSuffix], c) P.params_type = function
    | Int name -> P.int name
    | Int_def (name, _) -> P.int name
    | Float name -> P.float name
    | Float_def (name, _) -> P.float name
    | Prod (p1, p2) -> P.prod (get_eliom_param p1) (get_eliom_param p2)
;;

let get_form_function (param_ : ('a, 'c) t) (arg : 'c) = 
    let rec f : type a c. (a, c) t -> c -> [> Html_types.input ] Eliom_content.Html.elt list = 
        Eliom_content.Html.D.(function
        | Int name -> (fun arg -> [txt name; br(); Form.input ~input_type:`Text ~name:arg Form.int; br(); br()])
        | Int_def (name, def) -> (fun arg -> [txt name; br(); Form.input ~input_type:`Text ~name:arg ~value:def Form.int; br(); br()])
        | Float name -> (fun arg -> [txt name; Form.input ~input_type:`Text ~name:arg Form.float; br(); br()])
        | Float_def (name, def) -> (fun arg -> [txt name; br(); Form.input ~input_type:`Text ~name:arg ~value:def Form.float; br(); br()])
        | Prod (p1, p2) -> (fun (a1, a2) -> (f p1 a1) @ (f p2 a2))
        )
    in
    Eliom_content.Html.D.([p ((f param_ arg) @ [Form.input ~input_type:`Submit ~value:"Analyze" Form.string])])
;;



