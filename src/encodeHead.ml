
open Basic
open EncodeBasic
open EncodeOperation.Open


let e_mac_style (mac_style : Value.Head.mac_style) : unit encoder =
  let open EncodeOperation in
  let open Value.Head in
  e_16bits [
    mac_style.bold;
    mac_style.italic;
    mac_style.underline;
    mac_style.outline;
    mac_style.shadow;
    mac_style.condensed;
    mac_style.extended;
  ]


let make (ihead : Intermediate.Head.t) : table ok =
  let d = ihead.Intermediate.Head.derived in
  let v = ihead.Intermediate.Head.value in
  let loc_format_num =
    let open Intermediate in
    match d.index_to_loc_format with
    | ShortLocFormat -> 0
    | LongLocFormat  -> 1
  in
  let table_version = !% 0x00010000 in
  let magic_number = !% 0x5F0F3CF5 in
  let font_direction_hint = 0 in
  let glyph_data_format = 0 in
  let enc =
    let open EncodeOperation in
    e_uint32 table_version      >>= fun () ->
    e_uint32 v.font_revision    >>= fun () ->
    e_uint32 (!% 0)             >>= fun () -> (* `checkSumAdjustment`: will be updated afterwards *)
    e_uint32 magic_number       >>= fun () ->
    e_uint16 v.flags            >>= fun () ->
    e_uint16 v.units_per_em     >>= fun () ->
    e_timestamp v.created       >>= fun () ->
    e_timestamp v.modified      >>= fun () ->
    e_int16 d.x_min             >>= fun () ->
    e_int16 d.y_min             >>= fun () ->
    e_int16 d.x_max             >>= fun () ->
    e_int16 d.y_max             >>= fun () ->
    e_mac_style v.mac_style     >>= fun () ->
    e_uint16 v.lowest_rec_ppem  >>= fun () ->
    e_int16 font_direction_hint >>= fun () ->
    e_int16 loc_format_num      >>= fun () ->
    e_int16 glyph_data_format   >>= fun () ->
    return ()
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_head;
    contents;
  }
