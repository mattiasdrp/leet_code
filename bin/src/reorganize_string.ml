module PrioQueue = struct
  type priority = int
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

  let empty = Empty

  let rec push elt prio t =
    match t with
    | Empty -> Node (prio, elt, Empty, Empty)
    | Node (p, e, left, right) ->
        if prio > p then Node (prio, elt, push e p right, left)
        else Node (p, e, push elt prio right, left)

  let rec remove_top = function
    | Empty -> raise Not_found
    | Node (_, _, left, Empty) -> left
    | Node (_, _, Empty, right) -> right
    | Node
        ( _,
          _,
          (Node (lprio, lelt, _, _) as left),
          (Node (rprio, relt, _, _) as right) ) ->
        if lprio > rprio then Node (lprio, lelt, remove_top left, right)
        else Node (rprio, relt, left, remove_top right)

  let pop = function
    | Empty -> raise Not_found
    | Node (prio, elt, _, _) as queue -> (elt, prio, remove_top queue)
end

let parse length l =
  let module CMap = Map.Make (Char) in
  let rec aux map seq =
    match seq () with
    | Seq.Nil -> map
    | Seq.Cons (hd, tl) ->
        aux
          (CMap.update hd
             (function None -> Some 1 | Some v -> Some (v + 1))
             map)
          tl
  in
  let map = aux CMap.empty l in
  CMap.fold
    (fun s freq pqueue ->
      if freq > (length + 1) / 2 then raise Exit
      else PrioQueue.push s freq pqueue)
    map PrioQueue.empty

(* Looks for the highest frequency available char
   If no such char can be found (all available char are
   preceded by an already found char), adds an idle task
   and increase the timer *)
let rec find_available index string prev pqueue =
  match PrioQueue.pop pqueue with
  | e, freq, pqueue ->
      if Char.equal e prev then
        match find_available index string prev pqueue with
        (* Remember to push e, freq in the queue if we don't return it *)
        | e', freq', pqueue -> (e', freq', PrioQueue.push e freq pqueue)
        | exception Not_found -> assert false
      else (e, freq, pqueue)

let schedule string pqueue =
  let rec aux index prev l pqueue =
    match find_available index string prev pqueue with
    | e, freq, pqueue ->
        let pqueue =
          if freq = 1 then pqueue else PrioQueue.push e (freq - 1) pqueue
        in
        let prev = e in
        aux (index + 1) prev (e :: l) pqueue
    | exception Not_found -> l |> List.to_seq |> String.of_seq
  in
  aux 0 ' ' [] pqueue

let main string =
  let seq = String.to_seq string in
  try parse (String.length string) seq |> schedule string with Exit -> ""
