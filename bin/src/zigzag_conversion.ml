type dir = Up | Down

let fill matrix string =
  let rows = Array.length matrix in
  let rec aux dir i row col =
    if i = String.length string then ()
    else if row = rows then aux Up i (row - 2) (col + 1)
    else if row = -1 then aux Down i (row + 2) (col - 1)
    else (
      matrix.(row).(col) <- string.[i];
      match dir with
      | Up -> aux dir (i + 1) (row - 1) (col + 1)
      | Down -> aux dir (i + 1) (row + 1) col)
  in
  aux Down 0 0 0

let read matrix length =
  let bytes = Bytes.create length in
  ignore
    (Array.fold_left
       (fun i string ->
         Array.fold_left
           (fun i -> function
             | ' ' -> i
             | c ->
                 Bytes.set bytes i c;
                 i + 1)
           i string)
       0 matrix);
  Bytes.to_string bytes

let convert string max_rows =
  if max_rows = 1 then string
  else
    let matrix =
      Array.init max_rows (fun _ ->
          Array.init (String.length string / 2) (fun _ -> ' '))
    in
    fill matrix string;
    read matrix (String.length string)
