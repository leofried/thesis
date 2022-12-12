open Util;;
open Data;;

let pareto_list ~(luck : float) ~(number_of_teams : int) ~(max_games : int) =
  Data.get_data_list ~luck ~number_of_teams ~max_games
  |> List.map (fun (name, data) -> (name, data.decay, Data.calculate_imbalance data true))
  |> Lists.pareto
;;

let print_pareto ~(luck : float) ~(number_of_teams : int) ~(max_games : int) : unit =
  List.iter
    (fun (name, decay, imbalance) -> 
      print_endline @@ "(" ^ Math.to_pct ~digits:2 decay ^ ", " ^ Math.to_pct ~digits:2 imbalance ^ "): " ^ name
    )
    (pareto_list ~luck ~number_of_teams ~max_games)
;;

let print_all ~(luck : float) ~(number_of_teams : int) ~(max_games : int) : unit =
  List.iter
    (fun (name, data) -> 
      print_endline @@
      "(" ^ 
      Math.to_pct ~digits:2 data.decay ^
      " [" ^
      Math.to_pct ~digits:2 data.margin ^
      "], " ^
      Math.to_pct ~digits:2 (Data.calculate_imbalance data false) ^
      " [" ^
      Bool.to_string data.is_fair ^
      "]): " ^ name
    )
    (List.sort (fun (_, d1) (_, d2) -> Float.compare d2.decay d1.decay) (Data.get_data_list ~luck ~number_of_teams ~max_games))
;;
