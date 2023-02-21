open Util;;
open Struct;;
open Prog;;

Rand.set_seed () ;;
print_endline "" ;;

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

let f, x = Args.parse args in f x;;