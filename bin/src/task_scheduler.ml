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

module SMap = Map.Make (String)

let parse l =
  let rec aux map = function
    | [] -> map
    | hd :: tl ->
        aux
          (SMap.update hd
             (function None -> Some 1 | Some v -> Some (v + 1))
             map)
          tl
  in
  let map = aux SMap.empty l in
  SMap.fold
    (fun s freq pqueue -> PrioQueue.push s freq pqueue)
    map PrioQueue.empty

(* Looks for the highest frequency available task
   If no such task can be found (all available tasks are
   too close to the previous same task), adds an idle task
   and increase the timer *)
let rec find_available t n prev pqueue =
  match PrioQueue.pop pqueue with
  | e, freq, pqueue -> (
      match SMap.find e prev with
      | t' when t - t' <= n -> (
          match find_available t n prev pqueue with
          (* Remember to push e, freq in the queue if we don't return it *)
          | e', freq', pqueue -> (e', freq', PrioQueue.push e freq pqueue)
          | exception Not_found -> ("idle", 0, PrioQueue.push e freq pqueue))
      | _ | (exception Not_found) -> (e, freq, pqueue))

let schedule n pqueue =
  let rec aux t prev l pqueue =
    match find_available t n prev pqueue with
    | e, freq, pqueue ->
        let pqueue =
          if freq <= 1 then pqueue else PrioQueue.push e (freq - 1) pqueue
        in
        let prev = if freq <= 1 then prev else SMap.add e t prev in
        aux (t + 1) prev (e :: l) pqueue
    | exception Not_found -> l
  in
  aux 0 SMap.empty [] pqueue

let main l n =
  if n = 0 then List.length l else parse l |> schedule n |> List.length
