module Lists = struct
  let rec top_of_list lst n = 
    if n = 0 then [], lst else
      match lst with
      | [] -> invalid_arg "Lists.top_of_list"
      | hd :: tl -> 
        let top, bot = top_of_list tl (n-1) in
        hd :: top, bot
  ;;
  
  let rec find x lst =
    match lst with
    | [] -> invalid_arg "Lists.find"
    | h :: t -> if x = h then 0 else 1 + find x t
  ;;
  
  let rec sum_two_lists (l1 : int list) (l2 : int list) : int list =
    match l1, l2 with
    | [], [] -> []
    | h1 :: t1, h2 :: t2 -> h1 + h2 :: sum_two_lists t1 t2
    | _ -> invalid_arg "Lists.sum_two_lists"
  ;;
  
  let to_string (stringify : 'a -> string) = function
    | [] -> "[]"
    | [i] -> "[" ^ (stringify i) ^ "]"
    | hd :: tl ->
      let rec f = function
      | [] -> "]"
      | hd :: tl -> ", " ^ (stringify hd) ^ f tl
      in "[" ^ (stringify hd) ^ f tl
  ;;
         
  let pareto (lst : ('a * float * float) list) : ('a * float * float) list =
    let rec pareto (best : float) (lst : ('a * float * float) list) : ('a * float * float) list = 
      match lst with
      | [] -> []
      | (str, one, two) :: lst ->
          if compare two best < 0 then
            (str, one, two) :: pareto two lst
          else
            pareto best lst
    in
    lst
    |> List.sort (fun (_, x, _) (_, y, _) -> Float.compare x y)
    |> pareto Float.max_float
  ;;
end

module Math = struct 
  let inc_array (arr : int array) (i : int) : unit = arr.(i) <- arr.(i) + 1;;

  let pos_sub (x : float) (y : float) = if x < y then 0. else x -. y;;

  let sqrt_int (x : int) = sqrt (Int.to_float x);;

  let divide_up (x : int) (y : int) : int = if x mod y = 0 then x / y else x / y + 1;;
  let divide_int_int   (x : int)   (y : int)   : float = Int.to_float x /. Int.to_float y;;
  let divide_int_float (x : int)   (y : float) : float = Int.to_float x /. y;;
  let divide_float_int (x : float) (y : int)   : float = x /. Int.to_float y;;

  let to_pct ?(digits : int = 0) (x : float) =
    let div = 10. ** (Int.to_float digits) in
    Float.to_string (Float.round (x *. div *. 100.) /. div) ^ "%"
  ;;

  let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
  ;;

  let rec gcd a b = if b = 0 then a else gcd b (a mod b);;
end;;

module Rand : sig 
  val get_gaussian : unit -> float;;
end = struct
  let next_gaussian = ref None;;

  let rec get_gaussian() : float =
    match !next_gaussian with
    | None ->
      let x = Random.float 2.0 -. 1.0 in
      let y = Random.float 2.0 -. 1.0 in
      let s = x*.x +. y*.y in
      if s > 1.0 then get_gaussian() else
        let c = sqrt (-2.0 *. (log s) /. s) in
        next_gaussian := Some (x *. c);
        y *. c 
    | Some r ->
      next_gaussian := None;
        r
  ;;
end

module Stats = struct
  let mean (lst : float list) : float =
    Math.divide_float_int (List.fold_left Float.add 0. lst) (List.length lst)
  ;;
  
  let stdev (lst : float list) : float =
    let mean = mean lst in
    lst
    |> List.fold_left (fun sum x -> sum +. (x -. mean) ** 2.0) 0.
    |> Fun.flip Math.divide_float_int (List.length lst - 1)
    |> sqrt
  ;;
  
  let stderr (lst : float list) : float = stdev lst /. Math.sqrt_int (List.length lst);;
  
  let normed_stdev (lst : float list) : float = (stdev lst) /. (List.fold_left Float.add 0. lst);;
  
  let mean_two (n1, mean1 : int * float) (n2, mean2 : int * float) : float =
    let m1 = Int.to_float n1 in
    let m2 = Int.to_float n2 in
    (mean1 *. m1 +. mean2 *. m2 ) /. (m1 +. m2)
  ;;
  
  let stdev_two (n1, mean1, stdev1 : int * float * float) (n2, mean2, stdev2 : int * float * float) : float =
    let m1 = Int.to_float n1 in
    let m2 = Int.to_float n2 in
    sqrt (((m1 -. 1.) *. stdev1 *. stdev1 +. (m2 -. 1.) *. stdev2 *. stdev2) /. (m1 +. m2 -. 1.) +.
    m1 *. m2 *. (mean1 -. mean2) *. (mean1 -. mean2) /. (m1 +. m2) /. (m1 +. m2 -. 1.))
  ;;
  
  let stderr_two (n1, mean1, stderr1 : int * float * float) (n2, mean2, stderr2 : int * float * float) : float =
    let m1 = Int.to_float n1 in
    let m2 = Int.to_float n2 in
    stdev_two (n1, mean1, stderr1 *. sqrt m1) (n2, mean2, stderr2 *. sqrt m2) /.
    (sqrt (m1 +. m2))
  ;;
  
  let binom_error_monte_carlo ~(accuracy : int) ~(iters : int) ~(cats : int) : float = 
    let rec f = function
      | 0 -> []
      | x ->
        let arr = Array.make cats 0 in
        for _ = 1 to iters do
          Math.inc_array arr (Random.int cats)
        done;
        normed_stdev (List.map Int.to_float (Array.to_list arr)) :: f (x - 1)
    in
    let data = f (Math.pow 10 accuracy) in
    mean data;;
  ;;
  
  let binom_error_formula ~(iters : int) ~(cats : int) : float = 1. /. Math.sqrt_int (iters * cats);;
  
  let sample (n : int) (lst : ('a * float) list) : 'a list =
    let rec take k = function
      | [] -> assert false
      | (v, p) :: tl -> if k < p then v else take (k -. p) tl
    in let rec f n lst =
      match n with
      | 0 -> []
      | _ ->
        let tot = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 lst in
        take (Random.float tot) lst :: (f (n - 1) lst)
    in f n lst
  ;;
end

module Tree = struct
  type 'a t =
    | Leaf of 'a
    | Branch of 'a t * 'a t
  ;;

  let rec to_string (stringy : 'a -> string) (tree : 'a t) : string =
    match tree with
    | Leaf x -> stringy x
    | Branch (t1, t2) -> "(" ^ to_string stringy t1 ^ " v " ^ to_string stringy t2 ^ ")"
  ;;    
end;;

module Team : sig 
  type t 

  val make : ?name:string -> ?skill:float -> unit -> t

  val set_luck : float -> unit

  val play_game : t -> t -> t * t

  val get_skill : t -> float
end = struct
  type t = {name : string; skill : float};;

  let team_index = ref 0;;

  let get_next_team_name () = "Team " ^ (team_index := !team_index + 1; Int.to_string !team_index);;

  let make ?(name = get_next_team_name()) ?(skill = Rand.get_gaussian()) () : t = {name; skill}

  let luck = ref 1.;;

  let set_luck (lck : float) = (luck := lck);;


  let play_game (t1 : t) (t2 : t) : t * t =
    let debug = false in
    let t1p = t1.skill +. Rand.get_gaussian() *. !luck in
    let t2p = t2.skill +. Rand.get_gaussian() *. !luck in
    let cmp = compare t1p t2p in
    let flp = Rand.get_gaussian() > 0. in
    match cmp > 0 || (cmp == 0 && flp) with
      | true -> if debug then print_endline (t1.name ^ " beat " ^ t2.name) else (); t1, t2
      | false -> if debug then print_endline (t2.name ^ " beat " ^ t1.name) else (); t2, t1
  ;;

  let get_skill (t : t) : float = t.skill;;
end;;

module Scheme = struct
  type t = {
    name : string;
    number_of_teams : int;
    max_games : int;
    is_fair : bool;
    run : Team.t list -> Team.t list;
  };;
end

module Round_robin : sig
  val make : cycles:int -> number_of_teams:int -> Scheme.t
end = struct
  let play_games (teams : Team.t list) (cycles : int) : int array array =
    let teams_arr = Array.of_list teams in

    let n = List.length teams in
    let wins_arr = Array.init n (fun _ -> Array.make n 0) in
    for i = 0 to n - 1 do
      let t1 = teams_arr.(i) in
      for j = i + 1 to n - 1 do
        let t2 = teams_arr.(j) in
        for _ = 1 to cycles do
          if fst @@ Team.play_game t1 t2 = t1 then
            wins_arr.(i).(j) <- (wins_arr.(i).(j)) + 1
          else
            wins_arr.(j).(i) <- (wins_arr.(j).(i)) + 1
        done
      done
    done;
    wins_arr
  ;;

  module IMap = Map.Make(Int);;
  type imap = int list IMap.t;;

  let score_team (games : int array array) (teams : int list) (m : imap) (t1 : int) : imap =
    let points = List.fold_left (fun x t2 -> x + games.(t1).(t2)) 0 teams in
    let new_lst =
      match IMap.find_opt points m with
      | None -> [t1]
      | Some lst -> t1 :: lst in
    IMap.add points new_lst m
  ;;

  let tiebreak (lst : int list) : int list =
    let nd = List.map (fun c -> (Random.bits (), c)) lst in
    let sond = List.sort compare nd in
    List.map snd sond
  ;;

  let rec rank_teams (teams : Team.t list) (indicies : int list) (games : int array array) : int list =
    let scores = List.fold_left (score_team games indicies) IMap.empty indicies in
    match IMap.cardinal scores with
    | 1 -> tiebreak indicies
    | _ ->
      let levels = snd @@ List.split @@ IMap.bindings @@ scores in
      List.fold_left (fun ranks level -> (rank_teams teams level games) @ ranks) [] levels
  ;;

  let run_round_robin (cycles : int) (teams : Team.t list) : Team.t list = 
    play_games teams cycles
    |> rank_teams teams (List.init (List.length teams) Fun.id)
    |> List.map (List.nth teams)
  ;;

  let make ~(cycles : int) ~(number_of_teams : int): Scheme.t =
    {
      name = Int.to_string number_of_teams ^ " team " ^ Int.to_string cycles ^ "-Round Robin";
      number_of_teams;
      max_games = number_of_teams - 1;
      is_fair = true;
      run = run_round_robin cycles;
    }
  ;;
end

module Bracket : sig
  val is_fair : int list -> int -> bool

  val make : bracket:int list -> Scheme.t

  val string_to_bracket : string -> int list

  val get_all_brackets : int -> int list list list
end = struct
  let rec convert (bracket : int list) (i : int) (arr : int Tree.t array) : unit =
    match bracket with
    | [1] -> ()
    | [] | [_] -> invalid_arg "Bracket.convert"
    | 0 :: tl -> convert tl i arr
    | x :: _ when x < 0 -> invalid_arg "Bracket.convert"
    | a :: b :: tl -> 
      let j = i - a + 1 in
      arr.(j) <- Branch (arr.(j), arr.(i));
      convert (a - 2 :: b + 1 :: tl) (i - 1) arr
  ;;
  
  let build_tree (bracket : int list) : int Tree.t =
    let n = List.fold_left ( + ) 0 bracket in
    let arr = Array.init n (fun i -> Tree.Leaf i) in
    convert bracket (n - 1) arr;
    arr.(0)
  ;;
  
  let rec run_bracket (tree : int Tree.t) (teams : Team.t list) : Team.t list =
    match tree with
    | Leaf i -> [List.nth teams i]
    | Branch (i, j) ->
      let top = run_bracket i teams in
      let bot = run_bracket j teams in
      let winner, loser = Team.play_game (List.hd top) (List.hd bot) in
      winner :: loser :: (List.tl top) @ (List.tl bot)
  ;;
  
  let rec count_games (bracket : int list) : int =
    match bracket with
    | 0 :: tl -> count_games tl
    | _ -> List.length bracket - 1
  ;;
  
  let rec is_fair (bracket : int list) (seed_size : int) : bool =
    match bracket with
    | [] -> true
    | hd :: tl -> if hd mod seed_size = 0 then is_fair tl seed_size else false
  ;;
  
  let make ~(bracket : int list) : Scheme.t =
    let number_of_teams = List.fold_left ( + ) 0 bracket in
    {
      name = Int.to_string number_of_teams ^ " team " ^ Lists.to_string Int.to_string bracket ^ "-bracket";
      number_of_teams;
      max_games = count_games bracket;
      is_fair = is_fair bracket number_of_teams;
      run = run_bracket (build_tree bracket);
    }
  ;;  

  let string_to_bracket str = List.map int_of_string (String.split_on_char '_' str);;
  
  let rec bracket_children (bracket : int list) : int list list =
    match bracket with
    | [] -> []
    | hd :: tl -> 
      let lst = List.map (fun lst -> 0 :: hd + List.hd lst :: List.tl lst) (bracket_children tl) in
      if hd = 0 then lst else (2 :: (hd - 1) :: tl) :: lst
  ;;
  
  let rec get_all_brackets (number_of_teams : int) : int list list list =
    match number_of_teams with
    | 1 -> [[[1]]]
    | n ->
      let lst = get_all_brackets (n-1) in
      lst
      |> List.hd
      |> List.map bracket_children
      |> List.flatten
      |> List.sort_uniq (List.compare Int.compare)
      |> Fun.flip List.cons lst
  ;;
  
end

module Pool_play : sig
  val make : number_of_teams:int -> pool_count:int -> bracket:int list -> Scheme.t

  val get_all_pools :
    number_of_teams:int ->
    pool_counts:int list -> 
    max_games:int -> 
      Scheme.t list

end = struct
  let rec make_pots (pool_count : int) (teams : Team.t list) : Team.t list list =
    if List.length teams <= pool_count then [teams] else
      let pot, ts = Lists.top_of_list teams pool_count in
      pot :: make_pots pool_count ts
  ;;
  
  let rec make_pools (pool_count : int) (pots : Team.t list list) : Team.t list list =
    match pots with
    | [] -> []
    | _ ->
      pots
      |> List.map List.tl
      |> List.filter ((<>) [])
      |> make_pools pool_count
      |> List.cons (List.map List.hd pots)
    ;;
  
  let rec make_seeds (pools : Team.t list list) : Team.t list =
    match pools with
    | [] -> []
    | _ ->
      pools
      |> List.map List.tl
      |> List.filter ((<>) [])
      |> make_seeds
      |> List.append (List.map List.hd pools)
  ;;
  
  let run_pool (pool : Team.t list) : Team.t list =
    (Round_robin.make ~number_of_teams:(List.length pool) ~cycles:1).run pool
  ;;
  
  let run_bracket (bracket : Scheme.t) (teams : Team.t list) : Team.t list =
    let teams_to_bracket = bracket.number_of_teams in
    let top = List.filteri (fun i _ -> i < teams_to_bracket) teams in
    let bottom = List.filteri (fun i _ -> i >= teams_to_bracket) teams in
    let ranks = bracket.run top in
    ranks @ bottom
  ;;
  
  let run_pool_to_bracket (pool_count : int) (bracket : Scheme.t) (teams : Team.t list) : Team.t list =
    teams
    |> make_pots pool_count
    |> make_pools pool_count
    |> List.map run_pool
    |> make_seeds
    |> run_bracket bracket
  ;;
  
  let make ~(number_of_teams : int) ~(pool_count : int) ~(bracket : int list) : Scheme.t =
    let bracket_scheme = Bracket.make ~bracket in
    {
      name = Int.to_string number_of_teams ^ " team " ^ Int.to_string pool_count ^ " pool format breaking to a " ^ bracket_scheme.name;
      number_of_teams;
      max_games = Math.divide_up number_of_teams pool_count + bracket_scheme.max_games - 1;
      is_fair = number_of_teams mod pool_count = 0 && Bracket.is_fair bracket pool_count;
      run = run_pool_to_bracket pool_count bracket_scheme;
    }
  ;;
  
  let get_all_pools ~number_of_teams ~pool_counts ~max_games : Scheme.t list =
    let all_brackets = List.flatten (Bracket.get_all_brackets number_of_teams) in
    pool_counts
    |> List.map (fun pool_count -> List.map (fun bracket -> make ~number_of_teams ~pool_count ~bracket) all_brackets)
    |> List.flatten
    |> List.filter (fun (s : Scheme.t) -> s.max_games <= max_games)
  ;;
end

module Data = struct
  type t = {
    scheme : Scheme.t;
    iters : int;
    decay : float;
    margin : float;
    seed_wins : int list;
  };;

  let calculate_imbalance (data : t) (fair_to_zero : bool) : float =
    let raw = Stats.normed_stdev (List.map Int.to_float data.seed_wins) in
    if fair_to_zero then
      if data.scheme.is_fair then 0.
      else max 0.0001 (raw -. (Stats.binom_error_formula ~iters:data.iters ~cats:data.scheme.number_of_teams))
    else
      max 0.0001 raw
  ;;
end

module Simulator : sig
  val sim_scheme : luck:float -> iters:int -> Scheme.t -> Data.t
end = struct
  let rec get_best_team_skill (teams : Team.t list) : float =
    match teams with
    | [] -> assert false
    | [t] -> Team.get_skill t
    | hd :: tl -> Float.max (Team.get_skill hd) (get_best_team_skill tl)
  ;;

  let rec run_sims (scheme : Scheme.t) (iters_left : int) (decays: float list) (seed_wins : int array) : float list =
    if iters_left = 0 then decays else
    let teams = List.init (scheme.number_of_teams) (fun _ -> Team.make ()) in
    let winner = List.hd @@ scheme.run teams in
    let decay = get_best_team_skill teams -. Team.get_skill winner in
    Math.inc_array seed_wins (Lists.find winner teams);
    print_endline @@ string_of_int iters_left;
    run_sims scheme (iters_left - 1) (decay :: decays) seed_wins
  ;;

  let sim_scheme ~(luck : float) ~(iters : int) (scheme : Scheme.t) : Data.t =
    Team.set_luck luck;
    let seed_wins = Array.make (scheme.number_of_teams) 0 in
    let sims = run_sims scheme iters [] seed_wins in
    {
      iters;
      decay = Stats.mean sims;
      margin = Stats.stderr sims;
      seed_wins = Array.to_list seed_wins;
      scheme = scheme;
    }
  ;;
end

module Params_specs = struct
  module type S = sig
    type 'a param_name;;
    type ('a, 'b, 'c) params_type constraint 'b = [< [`WithoutSuffix | `WithSuffix | `Endsuffix]];;

    val int : string -> (int, [ `WithoutSuffix ], [ `One of int ] param_name) params_type
    val prod : ('a, [ `WithoutSuffix ], 'b) params_type ->
      ('c, [< `Endsuffix | `WithoutSuffix ] as 'd, 'e) params_type ->
      ('a * 'c, 'd, 'b * 'e) params_type
  end

  module M (X : S) = struct
    type (_, _) t = 
        | Int : string -> (int, [ `One of int] X.param_name) t
        | Int_def : string * int -> (int, [ `One of int] X.param_name) t
        | Prod : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
    ;;
        
    let rec get_eliom_param : type a c. (a, c) t -> (a, [`WithoutSuffix], c) X.params_type = function
        | Int name -> X.int name
        | Int_def (name, _) -> X.int name
        | Prod (p1, p2) -> X.prod (get_eliom_param p1) (get_eliom_param p2)
    ;;
  end
end

let f ~(luck : float) ~(iters : int) (scheme : Scheme.t) =
  let data = Simulator.sim_scheme ~luck ~iters scheme in
  [
    "Hyperparameters: iters = " ^ string_of_int iters ^ ", luck = " ^ string_of_float luck;
    "Format: " ^ scheme.Scheme.name ^ ".";
    "Decay: " ^ Math.to_pct ~digits:2 data.decay ^ " (" ^ Math.to_pct ~digits:2 data.margin ^ ")" ;
    "Imbalance: " ^ Math.to_pct ~digits:2 (Data.calculate_imbalance data false) ^ " (" ^ Math.to_pct ~digits:2 (Stats.binom_error_formula ~iters ~cats:scheme.number_of_teams) ^ ")"
  ]
;;