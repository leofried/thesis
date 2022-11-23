type t = Team.t option;;

let play_game (b1 : t) (b2 : t) : t * t =
  match b1, b2 with
  | None, None -> None, None
  | None, Some t2 -> Some t2, None
  | Some t1, None -> Some t1, None
  | Some t1, Some t2 ->
    let winner, loser = Team.play_game t1 t2 in
    Some winner, Some loser
;;

let is_bye : t -> bool = Option.is_none;;
let get_team : t -> Team.t = Option.get;;

let fill (teams : Team.t list) : t list = 
  let n = List.length teams in
  let rounds = Float.to_int @@ Float.ceil @@ Float.log(Int.to_float n) /. Float.log(2.) in
  let byes = Util.pow 2 rounds - n in
  List.map Option.some teams @ List.init byes (fun _ -> None)
;;

let empty (teams : t list) : Team.t list = List.filter_map Fun.id teams;;