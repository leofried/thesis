open Util;;
open Struct;;

Rand.set_seed () ;;
print_endline "" ;;

let number_of_teams = 24;;
let number_advance = 1;;

let max_games = 9;;
let specs = {Data.default_specs with number_of_teams; number_advance; max_games};;



Data.print specs;;
Simulate.simulate_schemes specs 1000 (Sptb.build_all specs);;
Simulate.simulate_smart_looped specs 100_000;;


(*
let lst = Sptb.build_all ~number_of_teams ~max_games:8 ~number_advance;;

List.iter (fun s -> 
  let stats = (Simulate.simulate s sp 1_000_000).stats in
  print_float @@ Stats.mean stats;
  print_string @@ " [";
  print_float @@ Stats.stderr stats;
  print_string @@ "] : ";
  print_endline @@ Scheme.name s;
) lst;;



let lst = Sptb.build_all ~number_of_teams:12 ~max_games:8 ~number_advance:1;;
print_endline @@ Lists.to_string Scheme.name true @@ lst;;
print_int (List.length lst);;



let s = Scheme.Chain [Pools (2, Round_robin); Offset (0, Bracket [4;2;0;0]); Offset (1, Bracket [1])];;
print_int (Scheme.max_games s 12);;
print_endline @@ Lists.to_string string_of_int false (Option.value (Scheme.get_symmetric_tiers s 12) ~default: [])

*)

(*let arg = [[16; 0; 0; 0; 0]; [8; 4; 0; 2; 0; 1; 0]];;
let s = Scheme.Format ((module (Chain.M (Bracket))), arg);;

let teams = Team.make_n (Scheme.number_of_teams s);;
print_endline @@ Lists.to_string (fun t -> t.Team.name) (Scheme.run s teams);;
print_endline "";;



let s : Scheme.t = Chain [
  Pools (2, Round_robin);
  Offset (0, Bracket [8; 0; 0; 0]);
  Offset (4, Bracket [8; 0; 2; 0; 0])
];;
print_endline @@ Scheme.name s

let teams = Team.make_n (Scheme.number_of_teams s);;
print_endline @@ Lists.to_string (fun t -> t.Team.name) (Scheme.run s teams);;
print_int (Scheme.max_games s 20);;
print_endline "";;

print_endline @@ string_of_bool (Scheme.is_symmetric s 20);;

let specs_menu = [
  "pool_play",
    [
      "max_games", Args.Int (Some Int.max_int); 
    (*  "pool_counts", Args.List None; *)
    ],
    (fun (getter : Args.getter) -> Scheme.get_all (module Pool_play)
      ~number_of_teams: (getter._int "number_of_teams")
     (* ~pool_counts: (getter._list "pool_counts") *)
      ~max_games: (getter._int "max_games")
    );
]


let args = Args.Menu [
  "", [],
    Args.Menu [
      "report",
        [
          "number_of_teams", Int None;
          "luck", Float (Some 1.);
          "max_games", Int (Some Int.max_int); 
        ], 
        Args.Menu [
          "pareto", [], Args.Final (fun getter -> Report.pareto
            ~number_of_teams: (getter._int "number_of_teams")
            ~luck: (getter._float "luck")
            ~max_games: (getter._int "max_games")
          );
          "all", [], Args.Final (fun getter -> Report.all
            ~number_of_teams: (getter._int "number_of_teams")
            ~luck: (getter._float "luck")
            ~max_games: (getter._int "max_games")
          );
        ];      
      "simulate",
        [
          "luck", Float (Some 1.); 
          "iters_pow", Int None;
        ],
        Args.Menu [
          "specs",
            [
              "file_name", String None;
            ],
            Args.Final (fun getter -> Simulate.sim_schemes
              ~luck: (getter._float "luck")
              ~iters_pow: (getter._int "iters_pow")
              (Specs.read (getter._string "file_name") true)
            );
          "smart",
            [
              "number_of_teams", Int None;
              "max_games", Int (Some Int.max_int);
              "batch_size", Int (Some 1);
            ],
            Args.Final (fun getter -> Simulate.sim_smart
              ~number_of_teams: (getter._int "number_of_teams")
              ~luck: (getter._float "luck")
              ~max_games: (getter._int "max_games")
              ~iters_pow: (getter._int "iters_pow")
              ~batch_size: (getter._int "batch_size")
            )
        ];
      "enumerate",
        [
          "number_of_teams", Int None;
          "file_name", String None;
          "overwrite", Bool (Some true);
        ],
        Args.Menu (
          List.map (fun (name, args, f) -> name, args,
            Args.Final (fun getter -> Specs.write
              ~file_name: (getter._string "file_name")
              ~overwrite: (getter._bool "overwrite")
              ~schemes: (f getter)
            )
          ) specs_menu
        );
      "pipeline",
        [
          "number_of_teams", Int None;
          "luck", Float (Some 1.); 
        ],
        Args.Menu (
          List.map (fun (name, args, f) -> name, args,
            Args.Final (fun getter ->
              let schemes = f getter in
              Simulate.sim_schemes
                ~luck: (getter._float "luck")
                ~iters_pow: 2
                schemes;
              Simulate.sim_smart
                ~number_of_teams: (getter._int "number_of_teams")
                ~luck: (getter._float "luck")
                ~max_games: (getter._int "max_games")
                ~iters_pow: 2
                ~batch_size: 500
            )
          ) specs_menu
        );
    ];
];;

let f, x = Args.parse args in f x;;*)