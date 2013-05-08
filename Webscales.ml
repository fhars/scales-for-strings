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

let draw_scale document table key' scale' fret' instr' _ =
  let tbody = Html.createTbody document in
  let key = value_or_zero key'
  and scale = value_or_zero scale'
  and fret = value_or_zero fret'
  and instrument = value_or_zero instr' in
  let tr = Html.createTr document in
  let td = Html.createTd document in
  let s = Printf.sprintf "%d %d %d %d" key scale fret instrument in
  append_text document td s;
  Dom.appendChild tr td;
  Dom.appendChild tbody tr;
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
  key##onchange <- (Html.handler redraw);
  scale##onchange <- (Html.handler redraw);
  fret##onchange <- (Html.handler redraw);
  instrument##onchange <- (Html.handler redraw);

     
