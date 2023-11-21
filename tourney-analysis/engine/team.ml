open! Util
open! Std

type t = {
  id : int;
  skill : float;
};;

let create (specs : Specs.t) =
  let id = ref 0 in

  specs.number_of_teams
  |> List.create
  |> List.map (fun _ -> Random.next_float specs.distr, Random.next_float Random.gaussian)
  |> List.sort_by_rev (fun (skill, seed) -> specs.fidel *. skill +. (1. -. specs.fidel) *. seed) Float.compare
  |> List.map (fun (skill, _) ->
    {
      id = Ref.incr id;
      skill;
    }
  )
;;

let play_game (specs : Specs.t) t1 t2 = 
  let debug = false in
  let t1p = Random.next_float (Normal (t1.skill, specs.luck)) in
  let t2p = Random.next_float (Normal (t2.skill, specs.luck)) in
  let cmp = Float.compare t1p t2p in
  match cmp > 0 || (cmp == 0 && Random.bool ()) with
    | true  -> if debug then Printf.printf "Team %d beat team %d (live)\n" t1.id t2.id else (); t1, t2
    | false -> if debug then Printf.printf "Team %d beat team %d (live)\n" t2.id t1.id else (); t2, t1
;;

let preconstruct ?(games = 10) (specs : Specs.t) =
  let teams = create specs in
  let arr = Array.make_matrix (specs.number_of_teams + 1) (specs.number_of_teams + 1) [] in
  for i = 1 to specs.number_of_teams do
    for j = 1 to specs.number_of_teams do
      if j > i then
        let t1 = List.nth teams (i - 1) in
        let t2 = List.nth teams (j - 1) in
        arr.(i).(j) <- List.create games |> List.map (fun _ -> play_game specs t1 t2)
      else
        arr.(i).(j) <- arr.(j).(i)
    done
  done;
  teams,
  (fun () ->
    let arr = Array.(map copy) arr in
    (fun t1 t2 ->
      match arr.(t1.id).(t2.id) with
      | [] -> assert false
      | hd :: tl ->
        arr.(t1.id).(t2.id) <- tl;
        arr.(t2.id).(t1.id) <- tl;
        if false then Printf.printf "Team %d beat team %d (recorded)\n" (fst hd).id (snd hd).id ;
        hd
    )
  )
;;

let skill t = t.skill;;

let sort = List.sort_by_rev skill Float.compare;;












(* type t = ((int * int) list) Array.t Array.t

let generate (specs : Specs.t) (g : int) : t list * u =
  let skills = 
    specs.number_of_teams
    |> List.create
    |> List.map (fun _ -> Random.next_float specs.distr, Random.next_float Random.gaussian)
    |> List.sort_by_rev (fun (skill, seed) -> specs.fidel *. skill +. (1. -. specs.fidel) *. seed) Float.compare
    |> List.map Pair.left
;;




let develop (t : t) =
  let t = Array.copy t in
  (fun t1 t2 ->
    match t.(t1).(t2) with
    | [] -> assert false
    | hd :: tl ->
      t.(t1).(t2) <- tl;
      t.(t2).(t1) <- tl;
      if true then print_endline ("Team " ^ fst hd ^ " beat team " ^ snd hd);
      hd
  )
;; *)
    







(*
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

let sort = List.sort_by_rev skill Float.compare;; *)