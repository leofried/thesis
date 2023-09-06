type t = Sexplib.Sexp.t [@@deriving sexp];;

let read = Sexplib.Sexp.load_sexp;;

let write file t = 
  let oc = open_out file in
  output_string oc (Sexplib.Sexp.to_string_hum t);
  close_out oc
;;

let of_string = Sexplib.Sexp.of_string;;

let to_string = Sexplib.Sexp.to_string;;

module type S = sig
  type t [@@deriving sexp]
end

let print (type a) (module M : S with type t = a) (x : a) =
  x
  |> M.sexp_of_t
  |> to_string
  |> print_endline
;;