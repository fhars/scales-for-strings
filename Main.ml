open Scales

let _ =
  let key = 6
  and offset = 0
  and instrument = guitar6 
  and (_, _, scale) = scales.(1) in
  let _width, _notes, filtered, base = generate_scale instrument key scale offset in
  List.iter2 (fun frets base -> 
    List.iter (fun n -> 
      if (n mod 12) = base then
        Printf.printf " *%d" n
      else
        Printf.printf "  %d" n
    ) frets; print_endline ""
  ) filtered base
