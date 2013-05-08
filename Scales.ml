let scales = [|
  "Lydian",     5, [ 0; 2; 4; 6; 7; 9; 11 ];
  "Ionian",     0, [ 0; 2; 4; 5; 7; 9; 11 ];
  "Mixolydian", 7, [ 0; 2; 4; 5; 7; 9; 10 ];
  "Dorian",     2, [ 0; 2; 3; 5; 7; 9; 10 ]; 
  "Aeolian",    9, [ 0; 2; 3; 5; 7; 8; 10 ];
  "Phrygian",   4, [ 0; 1; 3; 5; 7; 8; 10 ];
  "Locrian",   11, [ 0; 1; 3; 5; 6; 8; 10 ];
  "Lydian 7b",  5, [ 0; 2; 4; 6; 7; 9; 10 ];
  "Alterd",    -1, [ 0; 1; 3; 4; 5; 8; 10 ];
  "Sym Dim",   -1, [ 0; 1; 3; 4; 6; 7; 9; 10 ]
|] 

let keys = [| "C"; "C#"; "D"; "Eb"; "E"; "F"; "F#"; "G"; "Ab"; "A"; "Bb"; "B" |]

let guitar6 = [ 52; 57; 62; 67; 71; 76 ]
let banjo4 = [60; 67; 74; 81]

let instruments = [|
  "Guitar", guitar6;
  "Tenor Banjo", banjo4
|]

let fret_for_note str note =
  let f = (note - str) mod 12 in
  if f < 0 then f + 12 else f

let base_notes instrument key =
  List.map (fun str -> fret_for_note str key) instrument

let transpose key scale =
  List.map (fun note -> let n = (note + key) mod 12 in if n < 12 then n + 12 else n) scale

let transpose_raw key scale =
  List.map (fun note -> note + key) scale

module S = Set.Make(struct type t = int let compare = compare end)
let of_list = List.fold_left (fun i s -> S.add s i) S.empty
 
let notes_on_string str scale =
  let frets = List.fold_right 
    (fun note frets -> 
      S.add (fret_for_note str note) frets)
    scale S.empty in
  List.rev (S.fold (fun k l -> k :: l) frets [])

let all_notes instrument scale key =
  let scale' = transpose key scale in
  List.map (fun str -> notes_on_string str scale') instrument

let max_diff instrument =
  let rec m_d_r acc last = function
  | [] -> acc
  | str::l ->
    m_d_r (max acc (str - last)) str l
  in
  match instrument with
  | [] -> -1
  | [_] -> 12
  | str::l ->  
      m_d_r (-1) str l

let less n = List.filter (fun i -> i < n)

let rec filter notes instrument max_diff =
  match notes, instrument with
  | [], [] -> []
  | [ns], [str] -> [less max_diff ns]
  | ns::notes', str::(str' :: _ as strs') -> less (str' - str) ns :: filter notes' strs' max_diff
  | _ -> assert(false)

let filtered_notes instrument scale key offset =
  let notes = all_notes instrument scale key 
  and width = max_diff instrument in
  width, notes, List.map (fun l -> List.sort compare (transpose_raw offset l)) (filter (List.map (transpose (-offset)) notes) instrument width)
    
let generate_scale instrument key scale offset =
  let width, notes, filtered = filtered_notes instrument scale key offset
  and base = base_notes instrument key in
  width, List.map of_list notes, filtered, base
