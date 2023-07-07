open Util;;
open Engine;;

(*true is clip vs full, true is two vs three*)
type t = {
  seeds : int list;
  consol : bool;
  clipped : bool
} [@@deriving yojson];;

let kind = "test_scheme";;

let to_string {seeds; consol; clipped} = "consol: " ^ string_of_bool consol ^ "clipped : " ^ string_of_bool clipped ^ ": " ^ Lists.to_string string_of_int seeds;;

let run  {seeds; consol; clipped} (teams : Team.t list) =
  let teams = List.sort (fun x y -> Float.compare y.Team.skill x.Team.skill) teams in

  let w1, l1 = Team.play_game false (List.nth teams ((List.nth seeds 0) - 1)) (List.nth teams ((List.nth seeds 1) - 1)) in
  let w2, l2 = Team.play_game false (List.nth teams ((List.nth seeds 2) - 1)) (List.nth teams ((List.nth seeds 3) - 1)) in
  let w3, l3 = Team.play_game false (List.nth teams ((List.nth seeds 4) - 1)) (List.nth teams ((List.nth seeds 5) - 1)) in
  let w4, l4 = Team.play_game false (List.nth teams ((List.nth seeds 6) - 1)) (List.nth teams ((List.nth seeds 7) - 1)) in

  let ww1, wl1 = Team.play_game false w1 w2 in
  let ww2, wl2 = Team.play_game false w3 w4 in

  let www, wwl = Team.play_game false ww1 ww2 in

  match clipped with
  | true ->
    let wlw, wll = Team.play_game false wl1 wl2 in
    begin match consol with
    | true -> 
      let two, three = Team.play_game false wlw wwl in
      [www; two; three; wll; l1; l2; l3; l4]
    | false -> 
      [www; wwl; wlw; wll; l1; l2; l3; l4]
    end
  | false ->
    let lw1, ll1 = Team.play_game false l1 l2 in
    let lw2, ll2 = Team.play_game false l3 l4 in
    let cw1, cl1 = Team.play_game false wl1 lw2 in
    let cw2, cl2 = Team.play_game false wl2 lw1 in
    let cww, cwl = Team.play_game false cw1 cw2 in

    begin match consol with
    | true ->
      let two, three = Team.play_game false wwl cww in
      [www; two; three; cwl; cl1; cl2; ll1; ll2]
    | false -> 
      [www; wwl; cww; cwl; cl1; cl2; ll1; ll2]
    end
;; 