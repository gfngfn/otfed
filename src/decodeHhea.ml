
open Basic
open DecodeOperation.Open
open DecodeBasic


let fetch_num_h_metrics (core : common_source_core) (table_directory : table_directory) =
  let open ResultMonad in
  DecodeOperation.seek_required_table table_directory Value.Tag.table_hhea >>= fun (offset, _length) ->
  let open DecodeOperation in
  d_uint16 |> run core (offset + 34)


let d_hhea : Intermediate.Hhea.t decoder =
  let open DecodeOperation in
  d_uint32 >>= fun version ->
  if version <> !%% 0x00010000L then
    err @@ UnknownTableVersion(version)
  else
    d_int16  >>= fun ascender ->
    d_int16  >>= fun descender ->
    d_int16  >>= fun line_gap ->
    d_uint16 >>= fun advance_width_max ->
    d_int16  >>= fun min_left_side_bearing ->
    d_int16  >>= fun min_right_side_bearing ->
    d_int16  >>= fun xmax_extent ->
    d_int16  >>= fun caret_slope_rise ->
    d_int16  >>= fun caret_slope_run ->
    d_int16  >>= fun caret_offset ->
    return Intermediate.Hhea.{
      value = Value.Hhea.{
        ascender;
        descender;
        line_gap;
        caret_slope_rise;
        caret_slope_run;
        caret_offset;
      };
      derived = {
        advance_width_max;
        min_left_side_bearing;
        min_right_side_bearing;
        xmax_extent;
      };
    }


let get (src : source) : Intermediate.Hhea.t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_hhea >>= fun (offset, _length) ->
  DecodeOperation.run common.core offset d_hhea
