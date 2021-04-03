
open Basic


module Error = EncodeError
module Subset = EncodeSubset


type 'a ok = ('a, Error.t) result

type table = {
  tag      : Value.Tag.t;
  contents : string;
  length   : int;
}


module Head = struct

  let make (loc_format : Intermediate.loc_format) (ihead : Intermediate.Head.t) : table ok =
    let d = ihead.Intermediate.Head.derived in
    let v = ihead.Intermediate.Head.value in
    let loc_format_num =
      let open Intermediate in
      match loc_format with
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
      e_int16 d.xmin              >>= fun () ->
      e_int16 d.ymin              >>= fun () ->
      e_int16 d.xmax              >>= fun () ->
      e_int16 d.ymax              >>= fun () ->
      e_uint16 v.mac_style        >>= fun () ->
      e_uint16 v.lowest_rec_ppem  >>= fun () ->
      e_int16 font_direction_hint >>= fun () ->
      e_int16 loc_format_num      >>= fun () ->
      e_int16 glyph_data_format   >>= fun () ->
      current
    in
    let open ResultMonad in
    enc |> EncodeOperation.run >>= fun (contents, length) ->
    return {
      tag = Value.Tag.table_head;
      contents;
      length;
    }

end


module Hhea = struct

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
      current
    in
    let open ResultMonad in
    enc |> EncodeOperation.run >>= fun (contents, length) ->
    return {
      tag = Value.Tag.table_hhea;
      contents;
      length;
    }

end
