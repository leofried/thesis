
type tiebreaker = (*this is after head to head*)
  | Random
  | WorstCase
[@@deriving sexp]
;;

type 'a t = {
  play : 'a -> 'a -> 'a * 'a;
  compare : 'a -> 'a -> int;
  tiebreaker : tiebreaker;
}