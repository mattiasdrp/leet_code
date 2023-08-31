let expand string i j =
  let rec aux i j =
    if i < 0 || j >= String.length string || string.[i] <> string.[j] then (
      Format.eprintf "%d, %d@." i j;
      j - i - 1)
    else aux (i - 1) (j + 1)
  in
  aux i j

let find string =
  let rec aux max_length i =
    if i = String.length string then max_length
    else
      let odd_length = expand string i i in
      let even_length = expand string i (i + 1) in
      aux (max max_length (max odd_length even_length)) (i + 1)
  in
  aux 0 0
