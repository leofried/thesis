type one_int   = [ `One of int] Eliom_parameter.param_name;;
type one_float = [ `One of float] Eliom_parameter.param_name;;
type one_string = [ `One of string] Eliom_parameter.param_name;;

type (_, _) t = 
    | Int : string -> (int, one_int) t
    | Int_def : string * int -> (int, one_int) t
    | Float : string -> (float, one_float) t
    | Float_def : string * float -> (float, one_float) t
    | String : string -> (string, one_string) t
    | Prod : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
;;

let ( ** ) a b = Prod (a, b);;
          
let rec get_eliom_param : type a c. (a, c) t -> (a, [`WithoutSuffix], c) Eliom_parameter.params_type = function
    | Int name -> Eliom_parameter.int name
    | Int_def (name, _) -> Eliom_parameter.int name
    | Float name -> Eliom_parameter.float name
    | Float_def (name, _) -> Eliom_parameter.float name
    | String (name) -> Eliom_parameter.string name
    | Prod (p1, p2) -> Eliom_parameter.prod (get_eliom_param p1) (get_eliom_param p2)
;;

let get_form_function (param_ : ('a, 'c) t) (arg : 'c) = 
    let rec f : type a c. (a, c) t -> c -> [> Html_types.input ] Eliom_content.Html.elt list = 
        Eliom_content.Html.D.(function
        | Int name -> (fun arg -> [txt name; br(); Form.input ~input_type:`Text ~name:arg Form.int; br(); br()])
        | Int_def (name, def) -> (fun arg -> [txt name; br(); Form.input ~input_type:`Text ~name:arg ~value:def Form.int; br(); br()])
        | Float name -> (fun arg -> [txt name; br(); Form.input ~input_type:`Text ~name:arg Form.float; br(); br()])
        | Float_def (name, def) -> (fun arg -> [txt name; br(); Form.input ~input_type:`Text ~name:arg ~value:def Form.float; br(); br()])
        | String name -> (fun arg -> [txt name; br(); Form.input ~input_type:`Text ~name:arg Form.string; br(); br()])
        | Prod (p1, p2) -> (fun (a1, a2) -> (f p1 a1) @ (f p2 a2))
        )
    in
    Eliom_content.Html.D.([p ((f param_ arg) @ [Form.input ~input_type:`Submit ~value:"Analyze" Form.string])])
;;




(************************* ELIOM SCHEME ***************)

type (_, _) s =
    | Round_robin : (int * int, one_int * one_int) s
    | Bracket : (string, one_string) s
;;

let f : type a b . (a, b) s -> a Scheme.s = function
    | Round_robin -> Round_robin
    | Bracket -> Bracket
;;

let p : type a b . (a, b) s -> (a, b) t = function
    | Round_robin -> (Int "number_of_teams") ** (Int_def ("cycles", 1))
    | Bracket -> (String "bracket")
;;
