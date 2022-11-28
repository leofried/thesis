type t = {name : string; skill : float};;

let team_index = ref 0;;

let get_next_team_name () = "Team " ^ (team_index := !team_index + 1; Int.to_string !team_index);;

let make ?(name = get_next_team_name()) ?(skill = Util.Rand.get_gaussian()) () : t = {name; skill}


let luck = ref 1.;;

let set_luck (lck : float) = (luck := lck);;


let play_game (t1 : t) (t2 : t) : t * t =
  let debug = false in
  let t1p = t1.skill +. Util.Rand.get_gaussian() *. !luck in
  let t2p = t2.skill +. Util.Rand.get_gaussian() *. !luck in
  let cmp = compare t1p t2p in
  let flp = Util.Rand.get_gaussian() > 0. in
  match cmp > 0 || (cmp == 0 && flp) with
    | true -> if debug then print_endline (t1.name ^ " beat " ^ t2.name) else (); t1, t2
    | false -> if debug then print_endline (t2.name ^ " beat " ^ t1.name) else (); t2, t1
;;

let get_skill (t : t) : float = t.skill;;

let compare (t1: t) (t2: t) : int = String.compare t1.name t2.name;;

