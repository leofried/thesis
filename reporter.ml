

let get_pareto_list ~(luck : float) ~(number_of_teams : int) ~(max_games : int) : (Scheme.t * float * float) list =
  Data.read_filter ~luck ~number_of_teams ~max_games
  |> List.map (fun (data : Data.t) -> data.scheme, data.decay, Data.calculate_imbalance data true)
  |> Lists.pareto
;;


let next_smart_schemes ~(luck : float) ~(number_of_teams : int) ~(max_games : int) ~(batch_size : int) : Scheme.t list =
  let pareto_list = get_pareto_list ~luck ~number_of_teams ~max_games in

  let rec p_helper (lst : (Scheme.t * float * float) list) (data : Data.t) : Scheme.t * float = 
    let imb = Data.calculate_imbalance data true in
    match lst with
    | [] -> assert false
    | (_, c, d) :: _ when d <= imb -> data.scheme, 1. /. Float.pow 2. ((data.decay -. c) /. data.margin)
    | _ :: tl -> p_helper tl data
  in

  Data.read_filter ~luck ~number_of_teams ~max_games
  |> List.map (p_helper pareto_list) 
  |> Stats.sample batch_size
;;
