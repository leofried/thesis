
type t = Sexplib.Sexp.t [@@deriving sexp];;

let read = Sexplib.Sexp.load_sexp;;

let to_string = Sexplib.Sexp.to_string;;

let write file t = 
  let oc = open_out file in
  output_string oc (Sexplib.Sexp.to_string_hum t);
  close_out oc
;;