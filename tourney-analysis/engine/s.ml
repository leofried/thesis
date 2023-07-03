module type SCHEME = sig
  type t [@@deriving yojson]

  val kind : string

  val run : t -> Team.t list -> Team.t list

  val to_string : t -> string
end