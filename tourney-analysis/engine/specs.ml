type t = { 
  number_of_teams : int;
  number_advance : int;
  luck : float;
  fidel : float;
  max_games : int;
};;

let default number_of_teams = {
  number_of_teams;
  number_advance = 1;
  luck = 1.;
  fidel = 0.;
  max_games = Int.max_int;
}