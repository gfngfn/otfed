
open Basic
open DecodeOperation.Open
open DecodeBasic


let fetch_num_of_long_ver_metrics (core : common_source_core) (table_directory : table_directory) =
  let open ResultMonad in
  match DecodeOperation.seek_table table_directory Value.Tag.table_vhea with
  | None ->
      return None

  | Some((offset, _length)) ->
      let res =
        let open DecodeOperation in
        d_uint16 |> run core (offset + 34)
      in
      res >>= fun num_of_long_ver_metrics ->
      return @@ Some(num_of_long_ver_metrics)


let d_vhea_metrics_1_0 : Value.Vhea.metrics decoder =
  let open DecodeOperation in
  d_int16  >>= fun ascent ->
  d_int16  >>= fun descent ->
  d_int16  >>= fun _line_gap ->
  return @@ Value.Vhea.Version1_0{ ascent; descent }


let d_vhea_metrics_1_1 : Value.Vhea.metrics decoder =
  let open DecodeOperation in
  d_int16 >>= fun vert_typo_ascender ->
  d_int16 >>= fun vert_typo_descender ->
  d_int16 >>= fun vert_typo_line_gap ->
  return @@ Value.Vhea.Version1_1{
    vert_typo_ascender;
    vert_typo_descender;
    vert_typo_line_gap;
  }


let d_vhea : Intermediate.Vhea.t decoder =
  let open DecodeOperation in
  d_uint32 >>= fun version ->
  begin
    if version = !%% 0x00010000L then
      d_vhea_metrics_1_0
    else if version = !%% 0x00011000L then
      d_vhea_metrics_1_1
    else
      err @@ UnknownTableVersion(version)
  end >>= fun metrics ->
  d_uint16 >>= fun advance_height_max ->
  d_int16  >>= fun min_top_side_bearing ->
  d_int16  >>= fun min_bottom_side_bearing ->
  d_int16  >>= fun y_max_extent ->
  d_int16  >>= fun caret_slope_rise ->
  d_int16  >>= fun caret_slope_run ->
  d_int16  >>= fun caret_offset ->
  return Intermediate.Vhea.{
    value = Value.Vhea.{
      metrics;
      caret_slope_rise;
      caret_slope_run;
      caret_offset;
    };
    derived = {
      advance_height_max;
      min_top_side_bearing;
      min_bottom_side_bearing;
      y_max_extent;
    };
  }


let get (src : source) : (Intermediate.Vhea.t option) ok =
  let open ResultMonad in
  let common = get_common_source src in
  match DecodeOperation.seek_table common.table_directory Value.Tag.table_vhea with
  | None ->
      return None

  | Some((offset, _length)) ->
      DecodeOperation.run common.core offset d_vhea >>= fun ivhea ->
      return @@ Some(ivhea)
