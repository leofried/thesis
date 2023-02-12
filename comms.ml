
let bundle ~server ~client ~init () =
  let rec f server client x = Lwt.bind (server (Json.wrap client x)) (f server client) in
  Lwt.async
  (fun () ->
      f server client (Json.u_of_t (init ()))
  )
;;

let wrap (f : Json.t -> Json.t) (x : Json.u) : Json.u Lwt.t = Lwt.return (Json.wrap f x);;

module Functions : sig
  val server : Json.t -> Json.t
  val client : Json.t -> Json.t
  val init : unit -> Json.t
end = Functions;;