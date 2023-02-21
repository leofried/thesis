exception NoSuchFormatsHaveBeen of string

module Scheme :
  sig
    include module type of Struct.Scheme
    val t_of_yojson : Yojson.Safe.t -> t
  end

val read : string -> bool -> Scheme.t list
val write : schemes:Scheme.t list -> file_name:string -> overwrite:bool -> unit
