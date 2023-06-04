
open Basic
open EncodeOperation.Open
open EncodeBasic


let e_vhea_table_version_and_metrics (metrics : Value.Vhea.metrics) : unit encoder =
  let open EncodeOperation in
  match metrics with
  | Value.Vhea.Version1_0{ ascent; descent } ->
      let line_gap = 0 in
      e_uint32 !% 0x00010000 >>= fun () ->
      e_int16 ascent         >>= fun () ->
      e_int16 descent        >>= fun () ->
      e_int16 line_gap       >>= fun () ->
      return ()

  | Value.Vhea.Version1_1{ vert_typo_ascender; vert_typo_descender; vert_typo_line_gap } ->
      e_uint32 !% 0x00011000 >>= fun () ->
      e_int16 vert_typo_ascender  >>= fun () ->
      e_int16 vert_typo_descender >>= fun () ->
      e_int16 vert_typo_line_gap  >>= fun () ->
      return ()


let e_vhea ~(number_of_long_ver_metrics : int) (ivhea : Intermediate.Vhea.t) : unit encoder =
  let open EncodeOperation in
  let d = ivhea.Intermediate.Vhea.derived in
  let v = ivhea.Intermediate.Vhea.value in
  let metric_data_format = 0 in
  e_vhea_table_version_and_metrics v.metrics >>= fun () ->
  e_uint16 d.advance_height_max              >>= fun () ->
  e_int16  d.min_top_side_bearing            >>= fun () ->
  e_int16  d.min_bottom_side_bearing         >>= fun () ->
  e_int16  d.y_max_extent                    >>= fun () ->
  e_int16  v.caret_slope_rise                >>= fun () ->
  e_int16  v.caret_slope_run                 >>= fun () ->
  e_int16  v.caret_offset                    >>= fun () ->
  e_int16  0                                 >>= fun () ->
  e_int16  0                                 >>= fun () ->
  e_int16  0                                 >>= fun () ->
  e_int16  0                                 >>= fun () ->
  e_int16  metric_data_format                >>= fun () ->
  e_uint16 number_of_long_ver_metrics        >>= fun () ->
  return ()


let make ~(number_of_long_ver_metrics : int) (ivhea : Intermediate.Vhea.t) : table ok =
  let open ResultMonad in
  e_vhea ~number_of_long_ver_metrics ivhea |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_vhea;
    contents;
  }
