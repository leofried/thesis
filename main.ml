Random.self_init ();;


let rr : (int * int) Scheme.s = (module Round_robin);;
let br : string Scheme.s = (module Bracket);; 

print_endline "begin";;
let data = List.init 20 (fun i -> Simulator.sim_scheme ~luck:1. ~iters:100 (Scheme.Format (rr, (i + 1, 1))));;
Data.write data;;