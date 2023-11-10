open! Util
open! Std

type s = Specs.t;;

(*TODO: unique ids / own create function legal? all zeros?*)
type t = {
  id : int;
  skill : float;
  mutable games : int;
  mutable opps : (int * int) list;
};;

let create specs () = {
  id = 0;
  skill = Random.next_float specs.Specs.distr;
  games = 0;
  opps = [];
};;

let create_n (specs : Specs.t) n =
  let id = ref 0 in

  n
  |> List.create
  |> List.map (fun _ -> Random.next_float specs.distr, Random.next_float Random.gaussian)
  |> List.sort_by_rev (fun (skill, seed) -> specs.fidel *. skill +. (1. -. specs.fidel) *. seed) Float.compare
  |> List.map (fun (skill, _) ->
    {
      id = Ref.incr id;
      skill;
      games = 0;
      opps = [];
    }
  )
;;

let play_game (specs : Specs.t) (*~is_bracket*) t1 t2 = 
  (* let is_bracket = false in
  if is_bracket then begin
    let new_games = 1 + max t1.games t2.games in
    t1.games <- new_games;
    t2.games <- new_games;
  end else begin
    t1.games <- t1.games + 1;
    t2.games <- t2.games + 1;
  end; *)

  t1.opps <- List.assoc_update t2.id (Option.fold 1 ((+) 1)) t1.opps;
  t2.opps <- List.assoc_update t1.id (Option.fold 1 ((+) 1)) t2.opps;

  let debug = false in
  let t1p = Random.next_float (Normal (t1.skill, specs.luck)) in
  let t2p = Random.next_float (Normal (t2.skill, specs.luck)) in
  let cmp = Float.compare t1p t2p in
  match cmp > 0 || (cmp == 0 && Random.bool ()) with
    | true  -> if debug then Printf.printf "Team %d beat team %d\n" t1.id t2.id else (); t1, t2
    | false -> if debug then Printf.printf "Team %d beat team %d\n" t2.id t1.id else (); t2, t1
;;

let skill t = t.skill;;

let sort = List.sort_by_rev skill Float.compare;;