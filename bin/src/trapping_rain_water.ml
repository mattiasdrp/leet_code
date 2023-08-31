type t = { prof : int; left : int; right : int }

let l =
  List.map
    (fun prof -> { prof; left = 0; right = 0 })
    [ 0; 1; 0; 2; 1; 0; 1; 3; 2; 1; 2; 1 ]

let prof_max proj l =
  let rec aux max_prof rev_list = function
    | [] -> rev_list
    | hd :: tl ->
        let max_prof = if hd.prof <= max_prof then max_prof else hd.prof in
        aux max_prof (proj hd max_prof :: rev_list) tl
  in
  aux 0 [] l

let left_and_right l =
  prof_max (fun e left -> { e with left }) l
  |> prof_max (fun e right -> { e with right })

let compute l =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl ->
        let acc = min hd.left hd.right - hd.prof + acc in
        aux acc tl
  in
  aux 0 l
