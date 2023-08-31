let concat extra_space words nb_words =
  let spaces = extra_space / (nb_words - 1) in
  let rest = extra_space mod (nb_words - 1) in
  let rec aux rest phrase = function
    | [] -> phrase
    | [ word ] -> phrase ^ word
    | word :: tl ->
        let spaces =
          String.make (if rest > 0 then spaces + 1 else spaces) ' '
        in
        aux (rest - 1) (phrase ^ word ^ spaces) tl
  in
  aux rest "" words

let fill_spaces max_width l =
  let rec aux first acc = function
    | [] -> acc
    | (words, nb_words, width) :: tl ->
        let words =
          if width + nb_words - 1 = max_width then String.concat " " words
          else
            let extra_space = max_width - width in
            match words with
            | [ w ] -> w ^ String.make extra_space ' '
            | _ ->
                if first then
                  String.concat " " words
                  ^ String.make (extra_space - (nb_words - 1)) ' '
                else concat extra_space words nb_words
        in
        aux false (words :: acc) tl
  in
  aux true [] l

let main l max_width =
  let rec aux (phrase, nb_words, width) acc = function
    | [] -> fill_spaces max_width ((List.rev phrase, nb_words, width) :: acc)
    | hd :: tl ->
        let l = String.length hd in
        if l + width + nb_words > max_width then
          aux ([ hd ], 1, l) ((List.rev phrase, nb_words, width) :: acc) tl
        else aux (hd :: phrase, nb_words + 1, l + width) acc tl
  in
  aux ([], 0, 0) [] l
