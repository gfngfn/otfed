
open Basic
open EncodeBasic


let make ~number_of_h_metrics (ihhea : Intermediate.Hhea.t) : table ok =
  let d = ihhea.Intermediate.Hhea.derived in
  let v = ihhea.Intermediate.Hhea.value in
  let table_version = !% 0x00010000 in
  let metric_data_format = 0 in
  let enc =
    let open EncodeOperation in
    e_uint32 table_version            >>= fun () ->
    e_int16  v.ascender               >>= fun () ->
    e_int16  v.descender              >>= fun () ->
    e_int16  v.line_gap               >>= fun () ->
    e_uint16 d.advance_width_max      >>= fun () ->
    e_int16  d.min_left_side_bearing  >>= fun () ->
    e_int16  d.min_right_side_bearing >>= fun () ->
    e_int16  d.xmax_extent            >>= fun () ->
    e_int16  v.caret_slope_rise       >>= fun () ->
    e_int16  v.caret_slope_run        >>= fun () ->
    e_int16  v.caret_offset           >>= fun () ->
    e_int16  0                        >>= fun () ->
    e_int16  0                        >>= fun () ->
    e_int16  0                        >>= fun () ->
    e_int16  0                        >>= fun () ->
    e_int16  metric_data_format       >>= fun () ->
    e_uint16 number_of_h_metrics      >>= fun () ->
    return ()
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_hhea;
    contents;
  }
