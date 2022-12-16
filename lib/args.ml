type getter = {
  _int : string -> int;
  _float : string -> float;
  _string : string -> string;
  _list : string -> int list;
}

type parameter =
  | Int of int option
  | Float of float option
  | String of string option
  | List of int list option

type spec =
  | Menu of (string * (string * parameter) list * spec) list
  | Final of (getter -> unit)

let build_getter (params : (string * (parameter * bool)) list) : getter =
  {
    _int = (fun (arg : string) ->
      match fst (List.assoc arg params) with
      | Int (Some x) -> x
      | _ -> System.error ()
    );
    _float = (fun (arg : string) ->
      match fst (List.assoc arg params) with
      | Float (Some x) -> x
      | _ -> System.error ()
    );
    _string = (fun (arg : string) ->
      match fst (List.assoc arg params) with
      | String (Some x) -> x
      | _ -> System.error ()
    );
    _list = (fun (arg : string) ->
      match fst (List.assoc arg params) with
      | List (Some x) -> x
      | _ -> System.error ()
    );
  }
;;

let run (specs : spec) : unit =
  let rec f (specs : spec) (args : string list) (params : (string * (parameter * bool)) list) : unit =
    match args with
    | [] -> begin match specs with
      | Final prog -> prog (build_getter params)
      | Menu _ -> System.error ()
      end 
    | rg :: rem_args -> 
      if rg.[0] <> '-' then  System.error ();
      let arg = String.sub rg 1 (String.length rg - 1) in
      match specs with
      | Final _ -> g specs arg rem_args params
      | Menu options ->
        match List.assoc_opt arg (List.map (fun (a, b, c) -> (a, (b, c))) options) with
        | Some (new_params, spec) -> f spec rem_args ((List.map (fun (a, b) -> a, (b, false)) new_params) @ params)
        | None -> g specs arg rem_args params
  and g (specs : spec) (arg : string) (args : string list) (params : (string * (parameter * bool)) list) : unit =
    match List.assoc_opt arg params with
    | None -> System.error()
    | Some (_, true) -> System.error()
    | Some (param, false) ->
      match param with
      | Int _ -> f specs (List.tl args) (
        (arg, (Int (Some (int_of_string (List.hd args))), true)) ::
        (List.remove_assoc arg params)
      )
      | Float _ -> f specs (List.tl args) (
        (arg, (Float (Some (float_of_string (List.hd args))), true)) ::
        (List.remove_assoc arg params)
      )
      | String _ -> f specs (List.tl args) (
        if (List.hd args).[0] = '-' then System.error() else
        (arg, (String (Some (List.hd args)), true)) ::
        (List.remove_assoc arg params)
      )
      | List _ -> h specs arg [] args (List.remove_assoc arg params)
  and h (specs : spec) (arg : string) (values : int list) (args : string list) (params : (string * (parameter * bool)) list) : unit =
    if (List.length args = 0) || (int_of_string_opt (List.hd args)) = None then
      f specs args ((arg, (List (Some (List.rev values)), true)) :: params)
    else
      h specs arg ((int_of_string (List.hd args)) :: values) (List.tl args) params
  in f specs ("-" :: (List.tl (Array.to_list Sys.argv))) []
;;



