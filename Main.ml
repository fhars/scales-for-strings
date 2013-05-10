open Scales

let _ =
  let key = 0
  and offset = 0
  and skip = 2
  and instrument = guitar6 
  and (_, _, scale) = scales.(1) in
  let _width, _notes, filtered, base = generate_scale instrument key scale offset skip in
  List.iter2 (fun frets base -> 
    List.iter (fun n -> 
      if (n mod 12) = key then
        Printf.printf " *%d" n
      else
        Printf.printf "  %d" n
    ) frets; print_endline ""
  ) filtered base
