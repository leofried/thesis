open Util;;
open Struct;;
open Prog;;

Rand.set_seed () ;;

let specs = Args.Menu [
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
            Args.Final (fun getter -> Simulate.sim_specs
              ~file_name: (getter._string "file_name")
              ~luck: (getter._float "luck")
              ~iters: (Math.pow 10 (getter._int "iters_pow"))
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
              ~iters: (Math.pow 10 (getter._int "iters_pow"))
              ~batch_size: (getter._int "batch_size")
            )
        ];
      "enumerate",
        [
          "file_name", String None;
          "overwrite", Bool (Some true);
        ],
        Args.Menu [
          "pool_play",
            [
              "number_of_teams", Int None;
              "max_games", Int (Some Int.max_int); 
              "pool_counts", List None;
            ],
            Args.Final (fun getter -> Specs.write
              ~file_name: (getter._string "file_name")
              ~overwrite: (getter._bool "overwrite")
              ~schemes: (Pool_play.get_all_pools
                ~number_of_teams: (getter._int "number_of_teams")
                ~pool_counts: (getter._list "pool_counts")
                ~max_games: (getter._int "max_games")
              ) 
            );
        ];
    ];
];;

let f, x = Args.parse specs in f x;;