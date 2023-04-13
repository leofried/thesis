
module Univ : sig
  type t
  val embed : unit -> ('a -> t) * (t -> 'a option)
end = struct
  type t = | Value : 'a -> t;;

  let embed : type a . unit -> (a -> t) * (t -> a option) = (fun () ->
    (
      (fun x -> Value x),
      (function | Value x -> x)
    )
  );;
end

module Variable = struct
  
  let f x = x 0;;
  let z = Fun.id;;

  let a x f = f (x + 1);;

  let aa x y f = f (x + y)


end


