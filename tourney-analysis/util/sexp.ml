include Sexplib.Sexp;;

let write file t = 
  let oc = open_out file in
  output_string oc (to_string t);
  close_out oc
;;