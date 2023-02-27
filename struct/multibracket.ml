open Util;;
open Infix;;

type argument = Bracket.argument list [@@deriving yojson];;

let kind = "multibracket";;
let name _ = assert false;;
let number_of_teams = List.mapi (fun i arg -> i + Bracket.number_of_teams arg) >> Lists.fold max;;
