Random.self_init ();;

let rr : 'a Scheme.s = (module Round_robin);;
let br : 'a Scheme.s = (module Bracket);; 

Data.write @@ List.map (Simulator.sim_scheme ~luck:1. ~iters:100) (Scheme.get_all ~number_of_teams:8 ~max_games:4 br);;
Data.write @@ List.map (Simulator.sim_scheme ~luck:1. ~iters:100) (Scheme.get_all ~number_of_teams:8 ~max_games:4 rr);;



module _ = Comms;;