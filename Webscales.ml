(* Copyright (c) 2013 Florian Hars

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*)

module Html = Dom_html

open Scales

let open_numbers = [| "·";"➀";"➁";"➂";"➃";"➄";"➅";"➆";"➇";"➈";"➉";"⑪";"⑫" |]
let black_numbers = [| "X";"✪";"➋";"➌";"➍";"➎";"➏";"➐";"➑";"➒";"➓";"⓫";"⓬" |]

let rec enum from_n to_n =
  if from_n > to_n then []
  else from_n :: enum (from_n + 1) to_n

let scale_groups = [
  "Standard Modes", enum 0 6;
  "Melodic Minor Modes", enum 7 13;
  "Hamonic Minor Modes", enum 14 20;
  "Double Harmonic Modes", enum 21 27;
  "Pentatonic Modes", enum 28 32;
  "Blues & Bebop", enum 33 39;
  "Modes of Limited Transposition", enum 40 61;
  "Other Scales", enum 62 66;
]

let js = Js.string
let append_text document e s = Dom.appendChild e (document##createTextNode (js s))
let set_value e v = e##value##set (js (string_of_int v)) 

let clear_children elt =
  let c = elt##childNodes in
  for i = c##length - 1 downto 0 do
    Js.Opt.iter (c##item (i)) (fun node -> Dom.removeChild elt node)
  done

let value_or_zero elt =
  let v = try int_of_string (Js.to_string elt##value) with _ -> 0 in
  if v >= 0 then v else 0

let rec draw_frets document nrs str tr length fret filtered =
  if fret <= length then
    let td = Html.createTd document in
    td##className <- js "fret";
    Dom.appendChild tr td;
    let note = fret + str in
    let f' = match filtered with
      | n :: f' when n = note ->
	append_text document td black_numbers.(nrs.(note mod 12));
	f'
      | _ ->
	append_text document td open_numbers.(nrs.(note mod 12));
	filtered
    in
    draw_frets document nrs str tr length (fret + 1) f'

let draw_row document tbody nrs length instrument filtered =
  let tr = Html.createTr document
  and td = Html.createTd document in
  append_text document td keys.(instrument mod 12);
  Dom.appendChild tbody tr;
  Dom.appendChild tr td;
  let td = Html.createTd document in
  td##className <- js "nut";
  Dom.appendChild tr td;
  let f' = match filtered with
    | k :: f' when k == instrument ->
      append_text document td black_numbers.(nrs.(instrument mod 12));
      f'
    | _ ->
      append_text document td open_numbers.(nrs.(instrument mod 12));
      filtered
  in
  draw_frets document nrs instrument tr length 1 f'

let rec draw_rows document tbody nrs length instrument filtered =
  match instrument, filtered with
  | i::is, f::fs ->
    draw_row document tbody nrs length i f;
    draw_rows document tbody nrs length is fs
  | _ -> ()

let concat delim l =
  let rec concat_r acc l =
    match l with
    | [] -> acc
    | s::l' -> concat_r (s ^ delim ^ acc) l'
  in
  match l with
  | [] -> ""
  | s::l' -> concat_r s l'

let skip_names = [| "0"; "1"; "2"; "3"; "4"; "5"; "oct/2";"11121" |]

let rec rep i n =
  if n = 0 then [] else i::(rep i (n - 1))

let oct_per_two instrument =
  let rec oct_r start instr =
    match instr with
    | _ :: str :: instr' ->
       0 :: (fret_for_note str start) :: oct_r str instr'
    | [_]  -> [ 0; 0 ]
    | [] -> [ 0 ]
  in
  match instrument with
  | start :: instr -> oct_r start instr
  | _ -> []

let sum = List.fold_left ( + ) 0
let rec drop n l =
  if n = 0 then l else
    match l with
    | [] -> l
    | _ :: tl -> drop (n - 1) tl

let draw_scale document table link key' scale' fret' instr' skip' _ =
  let tbody = Html.createTbody document in
  let key = value_or_zero key'
  and scale_nr = value_or_zero scale'
  and instr = value_or_zero instr'
  and fret = value_or_zero fret'
  and skip = value_or_zero skip'
  and left_handed = false in

  let name, _, scale = scales.(scale_nr)
  and instr_name, instrument = instruments.(instr) in
  let skips = if skip <= 5 then
      rep skip (List.length instrument)
    else if skip = 6 then
      oct_per_two instrument
    else
      drop (7 - List.length instrument) [1;1;1;1;2;1;1]
  in

  let settings = ref [] in
  if key <> 0 then
    settings := ("key=" ^ (string_of_int key)) :: !settings;
  if scale_nr <> 0 then
    settings := ("scale=" ^ (string_of_int scale_nr)) :: !settings;
  if instr <> 0 then
    settings := ("instr=" ^ (string_of_int instr)) :: !settings;
  if fret <> 0 then
    settings := ("fret=" ^ (string_of_int fret)) :: !settings;
  if skip <> 0 then
    settings := ("skip=" ^ (string_of_int skip)) :: !settings;
  let frag = js ("#" ^ (concat "," !settings)) in
  link##href <- frag;
  let h = Html.window##location##hash in
  let h = if Js.to_string h = "" then js "#" else h in
  if (h <> frag) then (
    let hist_title = js (keys.(key) ^ " " ^ name ^ ", " ^ instr_name ^ ", fret " ^ (string_of_int fret)) in
    Html.window##document##title <- hist_title;
    (try
       Html.window##history##pushState((),hist_title,(Js.some frag))
     with
     | _ -> () (* Stupid old IE *)
    ));
  let width, filtered = generate_scale instrument key scale fret skips in
  let nrs = Array.create 12 0 in
  List.iteri (fun i off -> let ix = (key + off) mod 12 in nrs.(ix) <- i + 1) scale;
  let filtered, instrument =
    if left_handed then
      filtered, instrument
    else
      List.rev filtered, List.rev instrument
  in
  let length = max 19 (fret + width + sum skips + 1) in
  let tr = Html.createTr document
  and td = Html.createTd document in
  Dom.appendChild tr td;
  let td = Html.createTd document in
  Dom.appendChild tr td;
  for k = 1 to length do
    let td = Html.createTd document in
    td##className <- js "hd";
    td##style##width <- js (Printf.sprintf "%dpx" (40 - k));
    append_text document td (string_of_int k);
    Dom.appendChild tr td
  done;
  Dom.appendChild tbody tr;
  draw_rows document tbody nrs length instrument filtered;
  clear_children table;
  Dom.appendChild table tbody;
  Js._false

let split delim s =
  let rec split_r acc s =
    let pos = try Some (String.index s delim) with Not_found -> None in
    match pos with
    | None -> List.rev (s :: acc)
    | Some i -> split_r (String.sub s 0 i :: acc) (String.sub s (i + 1) (String.length s - i -1))
  in
  split_r [] s

let sel_key = ref 0
and sel_scale = ref 0
and sel_fret = ref 0
and sel_instr = ref 0
and sel_skip = ref 0

let parse_fragment () =
  sel_key := 0;
  sel_scale := 0;
  sel_fret := 0;
  sel_instr := 0;
  sel_skip := 0;
  let frag = Js.to_string Html.window##location##hash in
  let frag = if String.length frag > 1 then String.sub frag 1 (String.length frag - 1) else frag in
  let fields = List.map (split '=') (split ',' frag) in
  List.iter (fun l ->
    match l with
    | f :: vs :: [] -> (
      try
	let v = int_of_string vs in
	if v >= 0 then
	  match f with
	  | "key" -> if v < Array.length keys then sel_key := v
	  | "scale" ->  if v < Array.length scales then sel_scale := v
	  | "fret" ->  if v < 19 then sel_fret := v
	  | "instr" -> if v < Array.length instruments then sel_instr := v
	  | "skip" -> if v < Array.length skip_names then sel_skip := v
	  |  _ -> ()
      with
      | _ -> ()
    )
    | _ -> () (* Not valid *)
  ) fields

let setup () =
  parse_fragment ();

  let d = Html.document in
  let div =
    Js.Opt.get (d##getElementById(Js.string "scales"))
      (fun () -> assert false) in
  let control = Html.createDiv d
  and result = Html.createTable d in
  Dom.appendChild div control;
  Dom.appendChild div result;
  let key = Html.createSelect ~name:(Js.string "key") d in
  for k = 0 to Array.length keys - 1  do
    let s = Html.createOption d in
    append_text d s keys.(k);
    if k = !sel_key then
      s##selected <- true;
    s##value <- js (string_of_int k);
    Dom.appendChild key s
  done;
  Dom.appendChild control key;
  let scale = Html.createSelect ~name:(Js.string "scale") d in
  List.iter (fun (name, sc) ->
    let group = Html.createOptgroup d in
    group##label <- js name;
    List.iter (fun k ->
      let (name, _, _) = scales.(k) in
      let s = Html.createOption d in
      append_text d s name;
      if k = !sel_scale then
	s##selected <- true;
      s##value <- js (string_of_int k);
      Dom.appendChild group s
    ) sc;
    Dom.appendChild scale group;
  ) scale_groups;
  Dom.appendChild control scale;
  let fret = Html.createSelect ~name:(Js.string "fret") d in
  for k = 0 to 19  do
    let s = Html.createOption d in
    append_text d s (string_of_int k);
    if k = !sel_fret then
      s##selected <- true;
    s##value <- js (string_of_int k);
    Dom.appendChild fret s
  done;
  Dom.appendChild control fret;

  let instrument = Html.createSelect ~name:(Js.string "instrument") d in
  for k = 0 to Array.length instruments - 1  do
    let name, _ = instruments.(k) in
    let s = Html.createOption d in
    append_text d s name;
    if k = !sel_instr then
      s##selected <- true;
    s##value <- js (string_of_int k);
    Dom.appendChild instrument s
  done;
  Dom.appendChild control instrument;

  let skip_l = Html.createLabel d in
  skip_l##htmlFor <- Js.string "skip";
  append_text d skip_l " Skip: ";
  Dom.appendChild control skip_l;

  let skip = Html.createSelect ~name:(Js.string "skip") d in
  for k = 0 to Array.length skip_names - 1 do
    let s = Html.createOption d in
    append_text d s skip_names.(k);
    if k = !sel_skip then
      s##selected <- true;
    s##value <- js (string_of_int k);
    Dom.appendChild skip s
  done;
  Dom.appendChild control skip;

  let link = Html.createA d in
  link##href <- js "#";
  append_text d link "Link";
  Dom.appendChild div link;

  let redraw = draw_scale d result link key scale fret instrument skip in
  key##onchange <- (Html.handler (fun _ -> redraw ()));
  scale##onchange <- (Html.handler (fun _ -> redraw ()));
  fret##onchange <- (Html.handler (fun _ -> redraw ()));
  instrument##onchange <- (Html.handler (fun _ -> redraw ()));
  skip##onchange <- (Html.handler (fun _ -> redraw ()));
  redraw ()

let restart _ =
  let d = Html.document in
  let div =
    Js.Opt.get (d##getElementById(Js.string "scales"))
      (fun () -> assert false) in
  clear_children div;
  setup ()

let _ =
  Html.window##onhashchange <- Html.handler restart;
  setup ()
