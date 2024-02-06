open! Util
open! Std

type t =
  | Old
  | New
[@@deriving sexp]

let kind = "mlq";;

let number_of_teams _ = 6;;

let bo3 play a b =
  let w1, l1 = play a b in
  let w2, _ = play a b in
  if w1 = w2 then w1, l1 else play a b
;;

let run t Game.{play; _} teams =
  match teams with
  | [t1;t2;t3;t4;t5;t6] ->
    let a1, a2 = bo3 play t1 t2 in
    let b1, b2 = bo3 play t3 t4 in
    let c1, c2 = bo3 play t5 t6 in

    let winnerA, loserA = play c1 b2 in
    let winnerB, loserB = play c2 a2 in
    let winnerC, loserC = play a1 winnerA in
    let winnerD, loserD = play b1 winnerB in
    let winnerE, loserE = play winnerC winnerD in

    [
      match t with
      | Old ->
        let winnerF, loserF = play loserA loserD in
        let winnerG, loserG = play loserB loserC in
        let winnerH, loserH = play winnerF winnerG in
        let winnerI, loserI = play winnerH loserE in
        [winnerE; winnerI; loserI; loserH; loserF; loserG]
      | New ->
        let winnerF, loserF = play loserA loserB in
        let winnerG, loserG = play loserC loserD in
        let winnerH, loserH = play winnerF loserE in
        let winnerI, loserI = play winnerG winnerH in
        [winnerE; winnerI; loserI; loserH; loserG; loserF]
    ]

  | _ -> assert false
;;
