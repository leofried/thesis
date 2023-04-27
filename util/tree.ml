type 'a t =
  | Leaf of 'a
  | Branch of 'a t * 'a t
;;

let rec map f = function
  | Leaf x -> Leaf (f x)
  | Branch (x, y) -> Branch ((map f x), (map f y))
;;

let rec fold f = function
  | Leaf x -> x
  | Branch (x, y) -> f (fold f x) (fold f y)
;;

let rec filter f = function
  | Leaf x -> if f x then Some (Leaf x) else None
  | Branch (x, y) -> match filter f x, filter f y with
    | Some w, Some z -> Some (Branch (w, z))
    | Some w, None | None, Some w -> Some w
    | None, None -> None
;;

let rec count f = function
  | Leaf x -> if f x then 1 else 0
  | Branch (x, y) -> count f x + count f y
;;

let rec for_all f = function
  | Leaf x -> f x
  | Branch (x, y) -> for_all f x && for_all f y
;;

let rec exists f = function
  | Leaf x -> f x
  | Branch (x, y) -> exists f x || exists f y
;;

let rec to_list = function
  | Leaf x -> [x]
  | Branch (x, y) -> to_list x @ to_list y
;;

let rec to_string f = function
  | Leaf x -> f x
  | Branch (x, y) -> "{" ^ to_string f x ^ ", " ^ to_string f y ^ "}"
;;

let color (lst : int t list) (number_of_colors : int) : int list =

  let rec get_nearby_colors target = function
    | Leaf (x, None) -> ([], x = target)
    | Leaf (x, Some c) -> ([x, c], false)
    | Branch (x, y) ->
      match get_nearby_colors target x, get_nearby_colors target y with
      | (l1, false), (l2, false) -> (l1 @ l2), false
      | (f, false), (t, true) | (t, true), (f, false) -> t @ (List.sort (Tuple.compare ~left:Int.compare) f), true
      | (_, true), (_, true) -> assert false
  in

  let get_farthest_color counts colors =
    let rec f arr colors =
      if Array.fold_left (+) 0 arr = 1 then Lists.find 1 (Array.to_list arr)
      else match colors with
      | [] -> let scores = List.map (fun (i, c) -> (float_of_int i) +. (Math.divide_int_int 1 c)) (List.combine (Array.to_list arr) counts) in
              Lists.find (Lists.fold max scores) scores
      | hd :: tl -> arr.(hd) <- 0; f arr tl
    in
    f (Array.make number_of_colors 1) colors
  in

  let color target c = map (fun (x, o) -> x, match o with
    | None -> if x = target then Some c else None
    | Some d -> Some d
  ) in

  let rec f i k lst =
    if i > k then lst else

    let color_counts = List.map (fun i ->
        Lists.fold (+) (List.map (count (fun (_, j) -> j = Some i)) lst)
      ) (List.init number_of_colors Fun.id)
    in

    f (i+1) k (List.map (fun t ->
      t 
      |> get_nearby_colors i
      |> Tuple.left
      |> List.split
      |> Tuple.right
      |> get_farthest_color color_counts
      |> Fun.flip (color i) t
    ) lst)
  in

  lst
  |> List.map (map (fun x -> x, None))
  |> f 0 (Lists.fold (+) (List.map (count (fun _ -> true)) lst))
  |> List.map to_list
  |> List.concat
  |> List.sort (Tuple.compare ~left:Int.compare)
  |> List.split
  |> Tuple.right
  |> List.map Option.get
;;