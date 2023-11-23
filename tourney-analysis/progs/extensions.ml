

let pop t = List.hd t, List.tl t;;

let rec grab n t = match n with
  | 0 -> pop t
  | n -> let item, lst = grab (n-1) (List.tl t) in item, List.hd t :: lst
;;


type 'a pair = 'a * 'a;;

type format =
  | Result of bool
  | Game of format pair
;;

let rec get_all_formats = function
  | 0 -> [Result true; Result false]
  | n ->
    let all_smaller = get_all_formats (n-1) in
    all_smaller
    |> List.map (fun x -> List.map (fun y -> Game (x, y)) all_smaller)
    |> List.flatten
;;



type extension =
  | Final
  | Next of int * extension pair
;;

let rec get_all_extensions = function
  | 0 -> [Final]
  | n ->
    let all_smaller = get_all_extensions (n-1) in
    all_smaller
    |> List.map (fun x -> List.map (fun y -> List.init n (fun i -> Next (i, (x, y)))) all_smaller)
    |> List.flatten
    |> List.flatten
;;


type sequence = bool list;;

let rec get_all_sequences = function
  | 0 -> [[]]
  | n ->
    get_all_sequences (n-1)
    |> List.map (fun x -> [true :: x; false :: x])
    |> List.flatten
;;





let rec get_winner (f : format) (s : sequence) =
  match f, s with
  | Result b, [] -> b
  | Game (f1, f2), hd :: tl -> if hd then get_winner f1 tl else get_winner f2 tl
  | _, _ -> assert false
;;

let rec permute (e : extension) (s : sequence) =
  match e with
  | Final -> []
  | Next (i, (e1, e2)) ->
    let g, s = grab i s in 
    g :: if g then permute e1 s else permute e2 s
;;

let check_0 f1 f2 e s = get_winner f1 (permute e s) = get_winner f2 s;;

let check_1 n f1 f2 e =
  n
  |> get_all_sequences
  |> List.for_all (check_0 f1 f2 e)
;;

let check_2 n f1 f2 =
  n
  |> get_all_extensions
  |> List.exists (check_1 n f1 f2)
;;

let extension_grid n =
  let formats = get_all_formats n in
  formats
  |> List.map (fun x -> List.map (fun y -> x, y) formats)
  |> List.map (List.map (fun (x, y) -> check_2 n x y))
  (* |> List.map (List.map (fun b -> if b then 1 else 0)) *)
;;

let equality_grid n =
  let grid = extension_grid n in
  let ell = List.length grid in
  List.init ell (fun i ->
    List.init ell (fun j ->
      List.combine (List.nth grid i) (List.nth grid j)
      |> List.exists (fun (b, c) -> b && c)
    )    
  )
;;


let check_symmetric grid =
  let n = List.length grid in
  let arr = grid |> List.map Array.of_list |> Array.of_list in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if arr.(i).(j) && not arr.(j).(i) then
        print_endline @@ string_of_int i ^ ", " ^ string_of_int j
      else
        ()
    done
  done
;;


let check_transitive grid =
  let n = List.length grid in
  let arr = grid |> List.map Array.of_list |> Array.of_list in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      for k = 0 to n - 1 do
        if arr.(i).(j) && arr.(j).(k) && not arr.(i).(k) then
          print_endline @@ string_of_int i ^ ", " ^ string_of_int j ^ ", " ^ string_of_int k
        else
          ()
      done
    done
  done
;;



let rec make_symmetric = function
  | [] -> []
  | hd :: tl ->
    let z, y = pop hd in
    let x, gg = tl |> List.map pop |> List.split in
    let xy = List.combine x y |> List.map (fun (x, y) -> x || y) in
    (z :: xy) :: (gg |> make_symmetric |> List.combine xy |> List.map (fun (x, y) -> x :: y))
;;


(* let get_friends grid i =
  let rec f queued stacked =
    match queued with
    | [] -> stacked
    | i :: tl ->
      List.nth grid i
      |> List.mapi (fun i x -> i, x)
      |> List.mapi (fun i x -> )

let get_connected_components n =
  let grid =
    n
    |> grid
    |> make_symmetric
    (* |> List.combine (List.init Fun.id n) *)
  in *)



(* let classes n = 
  let f = function
  | [] -> 0
  | hd :: tl ->


 *)
