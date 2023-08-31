module CSet = Set.Make (Char)

let longest_substring s =
  let cset, longest =
    String.fold_left
      (fun (cset, longest) c ->
        if CSet.mem c cset then
          (CSet.singleton c, max (CSet.cardinal cset) longest)
        else (CSet.add c cset, longest))
      (CSet.empty, 0) s
  in
  max (CSet.cardinal cset) longest
