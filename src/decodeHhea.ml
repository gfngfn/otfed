
open Basic
open DecodeBasic


type derived = {
  advance_width_max      : int;
  min_left_side_bearing  : int;
  min_right_side_bearing : int;
  xmax_extent            : int;
}
[@@deriving show { with_path = false }]

type t = {
  value   : Value.Hhea.t;
  derived : derived;
}
[@@deriving show { with_path = false }]


let get (src : source) : t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_hhea >>= fun (offset, _length) ->
  let dec =
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
      return {
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
  in
  DecodeOperation.run common.core offset dec
