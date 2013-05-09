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

let js = Js.string
let append_text document e s = Dom.appendChild e (document##createTextNode (js s))
let set_value e v = e##value##set (js (string_of_int v)) 

let clear_children elt =
  let c = elt##childNodes in
  for i = 0 to c##length - 1 do
    Js.Opt.iter (c##item (i)) (fun node -> Dom.removeChild elt node)
  done

let value_or_zero elt =
  try int_of_string (Js.to_string elt##value) with _ -> 0

let rec draw_frets document tr length fret base notes filtered =
  if fret <= length then
    let td = Html.createTd document in
    td##className <- js "fret";
    Dom.appendChild tr td;
    let f' = match filtered with
      | n :: f' when n == fret ->
	append_text document td (if fret mod 12 == base then "◉" else "●");
	f'
      | _ ->
	append_text document td (if S.mem (fret mod 12) notes then "◌" else "·");
	filtered
    in
    draw_frets document tr length (fret + 1) base notes f'

let draw_row document tbody length instrument base notes filtered =
  let tr = Html.createTr document
  and td = Html.createTd document in
  append_text document td keys.(instrument mod 12);
  Dom.appendChild tbody tr;
  Dom.appendChild tr td;
  let td = Html.createTd document in
  td##className <- js "nut";
  Dom.appendChild tr td;
  let f' = match filtered with
    | 0 :: f' ->
      append_text document td (if base == 0 then "◉" else "●");
      f'
    | _ ->
      if S.mem 0 notes then
	append_text document td "◌"
      else
	append_text document td "·";
      filtered
  in
  draw_frets document tr length 1 base notes f'

let rec draw_rows document tbody length instrument base notes filtered =
  match instrument, base, notes, filtered with
  | i::is, b::bs, n::ns, f::fs ->
    draw_row document tbody length i b n f;
    draw_rows document tbody length is bs ns fs
  | _ -> ()

let draw_scale document table key' scale' fret' instr' _ =
  let tbody = Html.createTbody document in
  let key = value_or_zero key'
  and name, _, scale = scales.(value_or_zero scale')
  and fret = value_or_zero fret'
  and instr_name, instrument = instruments.(value_or_zero instr')
  and left_handed = false in
  let width, notes, filtered, base = generate_scale instrument key scale fret in
  let notes, filtered, base, instrument =
    if left_handed then
      notes, filtered, base, instrument
    else
      List.rev notes, List.rev filtered, List.rev base, List.rev instrument
  in
  let length = max 19 (fret + width) in
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
  draw_rows document tbody length instrument base notes filtered;
  clear_children table;
  Dom.appendChild table tbody;
  Js._false

let _ =
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
    s##value <- js (string_of_int k);
    Dom.appendChild key s
  done;
  Dom.appendChild control key;
  let scale = Html.createSelect ~name:(Js.string "scale") d in
  for k = 0 to Array.length scales - 1  do
    let (name, _, _) = scales.(k) in
    let s = Html.createOption d in
    append_text d s name;
    s##value <- js (string_of_int k);
    Dom.appendChild scale s
  done;
  Dom.appendChild control scale;
  let fret = Html.createSelect ~name:(Js.string "fret") d in
  for k = 0 to 24  do
    let s = Html.createOption d in
    append_text d s (string_of_int k);
    s##value <- js (string_of_int k);
    Dom.appendChild fret s
  done;
  Dom.appendChild control fret;
  let instrument = Html.createSelect ~name:(Js.string "instrument") d in
  for k = 0 to Array.length instruments - 1  do
    let name, _ = instruments.(k) in
    let s = Html.createOption d in
    append_text d s name;
    s##value <- js (string_of_int k);
    Dom.appendChild instrument s
  done;
  Dom.appendChild control instrument;
  let redraw = draw_scale d result key scale fret instrument in
  key##onchange <- (Html.handler (fun _ -> redraw ()));
  scale##onchange <- (Html.handler (fun _ -> redraw ()));
  fret##onchange <- (Html.handler (fun _ -> redraw ()));
  instrument##onchange <- (Html.handler (fun _ -> redraw ()));
  redraw ()

     
