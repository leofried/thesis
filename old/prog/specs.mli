exception NoSuchFormatsHaveBeen of string

val json_to_scheme : Util.Json.t -> Struct.Scheme.t

val read : string -> bool -> Struct.Scheme.t list
val write : schemes:Struct.Scheme.t list -> file_name:string -> overwrite:bool -> unit
