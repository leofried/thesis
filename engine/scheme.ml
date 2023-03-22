module type S = sig
  type t [@@deriving yojson]

  val kind : string

  val to_string : t -> string
  val run : t -> Team.t list -> Team.t list
end