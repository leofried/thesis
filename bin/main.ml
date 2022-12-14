open Util;;
Rand.set_seed () ;;


Args.run @@ Args.Menu [
  "",
    [
      "number_of_teams", Int None;
      "luck", Float (Some 1.);
      "max_games", Int (Some Int.max_int); 
    ],
    Args.Menu [
      "report", [], 
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
          "iters_pow", Int None;
        ],
        Args.Menu [
          "pool_play",
            [
              "pool_counts", List None;
            ],
            Args.Final (fun getter -> Simulate.sim_schemes
              ~luck: (getter._float "luck")
              ~iters: (Math.pow 10 (getter._int "iters_pow"))
              (Pool_play.get_all_pools
                ~number_of_teams: (getter._int "number_of_teams")
                ~pool_counts: (getter._list "pool_counts")
                ~max_games: (getter._int "max_games"))
                (*make vs make_from_json*)
          );
          "smart", [], Args.Final (fun getter -> Simulate.sim_smart
            ~number_of_teams: (getter._int "number_of_teams")
            ~luck: (getter._float "luck")
            ~max_games: (getter._int "max_games")
            ~iters: (Math.pow 10 (getter._int "iters_pow"))
          )
        ]
    ]
]