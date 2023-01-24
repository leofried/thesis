type t = {
    name : string;
    number_of_teams : int;
    max_games : int;
    is_fair : bool;
    run : Team.t list -> Team.t list;
};;