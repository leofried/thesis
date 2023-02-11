open Infix;;

let luck = 1.;;
let number_of_teams = 12;;
let max_games = 6;;










let x = ref 0;;


let server (data : Json.t) : Json.t =
  data
  |> Json.rip_list Data.t_of_yojson
  |> Data.write;
  incr x;
  [
    {
      scheme = Scheme.Format ((module Round_robin), (!x, 1));
      luck = 2.;
      iters = 10;
      margin = 0.;
      decay = 0.;
      seed_wins = []
    };
  ]
  |> Json.place_list Data.yojson_of_t
;;

let y = ref 0;;
let client : Json.t -> Json.t = fun x ->
incr y;
print_endline (string_of_int !y);
  (
    Json.rip_list Data.t_of_yojson
    >> List.map Data.(fun data -> Simulator.sim_scheme ~luck:data.luck ~iters:data.iters data.scheme)
    >> Json.place_list Data.yojson_of_t
  ) x
;;

let init () : Json.t = print_endline "Connected";`List [] ;;