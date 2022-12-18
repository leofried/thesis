type getter = {
  _bool : string -> bool;
  _int : string -> int;
  _float : string -> float;
  _string : string -> string;
  _list : string -> int list;
};;

type parameter =
  | Bool of bool option
  | Int of int option
  | Float of float option
  | String of string option
  | List of int list option
;;

type spec =
  | Menu of (string * (string * parameter) list * spec) list
  | Final of (getter -> unit)
;;

exception Exception of string

let fail str = raise (Exception str);;
let fail_prog str = raise (Exception ("Programmer error: " ^ str));;

let build_getter (params : (string * (parameter * bool)) list) : getter =
  List.iter (fun (s, (p, _)) ->
    if p = Int None || p = Float None || p = String None || p = List None then 
      fail ("Argument -" ^ s ^ " was not defined.")
    ) params;
  {
    _bool = (fun (arg : string) ->
      match fst (List.assoc arg params) with
      | Bool (Some x) -> x
      | _ -> fail_prog ("argument -" ^ arg ^ "does not have type bool.")
    );
    _int = (fun (arg : string) ->
      match fst (List.assoc arg params) with
      | Int (Some x) -> x
      | _ -> fail_prog ("argument -" ^ arg ^ "does not have type int.")
    );
    _float = (fun (arg : string) ->
      match fst (List.assoc arg params) with
      | Float (Some x) -> x
      | _ -> fail_prog ("argument -" ^ arg ^ "does not have type float.")
    );
    _string = (fun (arg : string) ->
      match fst (List.assoc arg params) with
      | String (Some x) -> x
      | _ -> fail_prog ("argument -" ^ arg ^ "does not have type string.")
    );
    _list = (fun (arg : string) ->
      match fst (List.assoc arg params) with
      | List (Some x) -> x
      | _ -> fail_prog ("argument -" ^ arg ^ "does not have type list.")
    );
  }
;;

let parse (specs : spec) : (getter -> unit) * getter =
  let rec f (specs : spec) (args : string list) (params : (string * (parameter * bool)) list) =
    match args with
    | [] -> begin match specs with
      | Final prog -> prog, (build_getter params)
      | Menu menu -> fail ("Menu option not selected. Options: " ^ Lists.to_string (fun (s, _, _) -> s) menu)
      end 
    | rg :: rem_args -> 
      if rg.[0] <> '-' then fail ("Expected argument, got '" ^ rg ^ "'");
      let arg = String.sub rg 1 (String.length rg - 1) in
      match specs with
      | Final _ -> g specs arg rem_args params
      | Menu options ->
        match List.assoc_opt arg (List.map (fun (a, b, c) -> (a, (b, c))) options) with
        | Some (new_params, spec) -> f spec rem_args ((List.map (fun (a, b) -> a, (b, false)) new_params) @ params)
        | None -> g specs arg rem_args params
  and g (specs : spec) (arg : string) (args : string list) (params : (string * (parameter * bool)) list) =
    match List.assoc_opt arg params with
    | None -> fail ("No such argument -" ^ arg ^ ". Options: " ^ 
      Lists.to_string Fun.id (
        (match specs with
          | Final _ -> []
          | Menu menu -> List.map (fun (s, _, _) -> s) menu
        ) @ List.filter_map (fun (n, (_, b)) -> if b then None else Some n) params
      ));
    | Some (_, true) -> fail ("Argument -" ^ arg ^ " was already defined.")
    | Some (param, false) ->
      if args = [] then fail ("No value provided to argument -" ^ arg ^ ".");
      let value = List.hd args in
      match param with
      | Bool _ -> 
        begin match bool_of_string_opt value with
        | None -> fail ("Argument -" ^ arg ^ " has type bool. Received: '" ^ value ^ "'.")
        | Some x -> f specs (List.tl args) (
          (arg, (Bool (Some x), true)) ::
          (List.remove_assoc arg params)
        ) end
      | Int _ -> 
        begin match int_of_string_opt value with
        | None -> fail ("Argument -" ^ arg ^ " has type int. Received: '" ^ value ^ "'.")
        | Some x -> f specs (List.tl args) (
          (arg, (Int (Some x), true)) ::
          (List.remove_assoc arg params)
        ) end
      | Float _ ->
        begin match float_of_string_opt value with
        | None -> fail ("Argument -" ^ arg ^ " has type float. Received: '" ^ value ^ "'.")
        | Some x -> f specs (List.tl args) (
          (arg, (Float (Some x), true)) ::
          (List.remove_assoc arg params)
        ) end
      | String _ ->
        if value.[0] = '-' then
          fail ("Argument -" ^ arg ^ " has type float. Received: '" ^ value ^ "'.")
        else
          f specs (List.tl args) (
            (arg, (String (Some value), true)) ::
            (List.remove_assoc arg params)
          )
      | List _ -> h specs arg [] args (List.remove_assoc arg params)
  and h (specs : spec) (arg : string) (values : int list) (args : string list) (params : (string * (parameter * bool)) list) =
    if (List.length args = 0) || (int_of_string_opt (List.hd args)) = None then
      f specs args ((arg, (List (Some (List.rev values)), true)) :: params)
    else
      match int_of_string_opt (List.hd args) with
      | None -> fail ("Argument -" ^ arg ^ " has type int list. Received: '" ^ List.hd args ^ "'.")
      | Some x -> h specs arg (x :: values) (List.tl args) params
  in f specs ("-" :: (List.tl (Array.to_list Sys.argv))) []
;;
