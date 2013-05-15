(* Copyright (c) 2013 Florian Hars

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*)

let scales = [|
  "Lydian",     5, [ 0; 2; 4; 6; 7; 9; 11 ];
  "Ionian",     0, [ 0; 2; 4; 5; 7; 9; 11 ];
  "Mixolydian", 7, [ 0; 2; 4; 5; 7; 9; 10 ];
  "Dorian",     2, [ 0; 2; 3; 5; 7; 9; 10 ]; 
  "Aeolian",    9, [ 0; 2; 3; 5; 7; 8; 10 ];
  "Phrygian",   4, [ 0; 1; 3; 5; 7; 8; 10 ];
  "Locrian",   11, [ 0; 1; 3; 5; 6; 8; 10 ];
  "Lydian b7",  5, [ 0; 2; 4; 6; 7; 9; 10 ];
  "Altered",   -1, [ 0; 1; 3; 4; 5; 8; 10 ];
  "Sym Dim",   -1, [ 0; 1; 3; 4; 6; 7; 9; 10 ]
	     |] 

let keys = [| "C"; "Db"; "D"; "Eb"; "E"; "F"; "F#/Gb"; "G"; "Ab"; "A"; "Bb"; "B" |]

let guitar6 = [ 52; 57; 62; 67; 71; 76 ]
let banjo4 = [60; 67; 74; 81]

let instruments = [|
  "Guitar", guitar6;
  "Guitar (open D)", [50; 57; 62; 66; 69; 74];
  "Guitar (open G)", [50; 55; 62; 67; 71; 74];
  "Guitar (drop D)", [ 50; 57; 62; 67; 71; 76 ];
  "Guitar (modal)", [ 50; 57; 62; 67; 69; 74 ];
  "Guitar (NST)", [ 48; 55; 62; 69; 76; 79 ];
  "Ukulele (C)", [ 67; 72; 76; 81 ];
  "Ukulele (D)", [ 69; 74; 78; 83 ];
  "Violin", [67; 74; 81; 88];
  "Tenor Banjo", banjo4
		  |]

let fret_for_note str note =
  let f = (note - str) mod 12 in
  if f < 0 then f + 12 else f

let base_notes instrument key =
  List.map (fun str -> fret_for_note str key) instrument

let transpose key scale =
  List.map (fun note -> let n = (note + key) mod 12 in if n < 0 then n + 12 else n) scale

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

let rec range from width =
  if width > 0 then
    from :: range (from + 1) (width - 1)
  else
    []

let in_range from width notes = List.filter (fun i -> S.mem (i mod 12) notes) (range from width)

let rec filter notes instrument max_diff skips accum_skip =
  match instrument, skips with
  | [], [] -> []
  | [str], [skip] -> [in_range (accum_skip + str) (max_diff + skip) notes]
  | str::(str' :: _ as strs'),  skip::skips' ->
    in_range (accum_skip + str) (str' - str + skip) notes :: filter notes strs' max_diff skips' (accum_skip + skip)
  | _ -> assert false

let generate_scale instrument key scale offset skips =
  let notes = of_list (transpose key scale)
  and width = max_diff instrument in
  let filtered = filter notes instrument width skips offset in
  width, filtered
