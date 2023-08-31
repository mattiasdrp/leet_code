(* Task scheduler *)
let () =
  Format.printf "@.%d@."
    (Task_scheduler.main
       [ "A"; "A"; "A"; "A"; "A"; "A"; "B"; "C"; "D"; "E"; "F"; "G" ]
       2)

(* Reorganize string *)
let () = Format.printf "@.'%s'@." (Reorganize_string.main "aab")

let () =
  Format.printf "@[<v 0>%a@."
    Format.(
      pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf s ->
          Format.fprintf ppf "'%s'" s))
    (Text_justification.main
       (* [ "What"; "must"; "be"; "acknowledgment"; "shall"; "be" ] *)
       [ "This"; "is"; "an"; "example"; "of"; "text"; "justification." ]
       16)

let () =
  Format.printf "%d@."
    (Longest_substring_no_repeating.longest_substring "abcdeabcdef")

let () = Format.printf "%d@." (Longest_palindromic_substring.find "babad")
