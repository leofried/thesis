type t = {
  id : int;
  skill : float;
  mutable games : int;
};;

let create ~(fidel : float) ~(n : int) =
  let id = ref 0 in

  n
  |> List.create
  |> List.map (fun _ -> Random.get_gaussian(), Random.get_gaussian())
  |> List.sort_by_rev (fun (skill, seed) -> fidel *. skill +. (1. -. fidel) *. seed) Float.compare
  |> List.map (fun (skill, _) ->
    {
      id = Ref.incr id;
      skill;
      games = 0
    }
  )
;;

let play_game ~luck ~is_bracket t1 t2 = 
  if is_bracket then begin
    let new_games = 1 + max t1.games t2.games in
    t1.games <- new_games;
    t2.games <- new_games;
  end else begin
    t1.games <- t1.games + 1;
    t2.games <- t2.games + 1;
  end;

  let debug = false in
  let t1p = t1.skill +. Random.get_gaussian() *. luck in
  let t2p = t2.skill +. Random.get_gaussian() *. luck in
  let cmp = Float.compare t1p t2p in
  match cmp > 0 || (cmp == 0 && Random.bool ()) with
    | true  -> if debug then Printf.printf "Team %d beat team %d\n" t1.id t2.id else (); t1, t2
    | false -> if debug then Printf.printf "Team %d beat team %d\n" t2.id t1.id else (); t2, t1
;;