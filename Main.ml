open Scales

let _ =
  let key = 4
  and instrument = guitar6 
  and (_, _, scale) = List.nth scales 1 in
  let notes = filtered_notes instrument scale key 2 
  and base = base_notes instrument key in
  List.iter2 (fun frets base -> 
    List.iter (fun n -> 
      if (n mod 12) = base then
        Printf.printf " *%d" n
      else
        Printf.printf "  %d" n
    ) frets; print_endline ""
  ) notes base
