
open Basic
open DecodeOperation


include DecodeBasic


let d_init_ttf core =
  d_structure >>= fun table_directory ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
    }
  in
  let ttf = {ttf_common = common} in
  return @@ (common, Ttf(ttf))


let d_init_cff (core : common_source_core) : source decoder =
  d_structure >>= fun table_directory ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
    }
  in
  let cff = {cff_common = common} in
  return @@ (common, Cff(cff))


let source_of_string (s : string) : single_or_collection ok =
  let core =
    {
      data = s;
      max  = String.length s - 1;
    }
  in
  let dec =
    d_format_version >>= fun format ->
    match format with
    | InitTtf ->
        d_init_ttf core >>= fun src ->
        return @@ Single(src)

    | InitCff ->
        d_init_cff core >>= fun src ->
        return @@ Single(src)

    | InitCollection ->
        d_ttc_header_offset_list >>= fun offsets ->
        offsets |> mapM (fun offset ->
          seek offset >>= fun () ->
          d_format_version >>= fun format ->
          match format with
          | InitTtf ->
              d_init_ttf core

          | InitCff ->
              d_init_cff core

          | InitCollection ->
              err LayeredTtc
        ) >>= fun srcs ->
        return @@ Collection(srcs)
  in
  run core 0 dec


let tables (common : common_source) : Value.Tag.t set =
  let acc =
    TableDirectory.fold (fun tag _ acc ->
      Alist.extend acc tag
    ) common.table_directory Alist.empty
  in
  acc |> Alist.to_list


module Intermediate = DecodeIntermediate


let cmap (common : common_source) : Intermediate.Cmap.t ok =
  let open ResultMonad in
  seek_required_table common Value.Tag.table_cmap >>= fun (offset, length) ->
  return @@ Intermediate.Cmap.make common.core offset length


let head (common : common_source) : Value.Head.t ok =
  let open ResultMonad in
  seek_required_table common Value.Tag.table_head >>= fun (offset, _length) ->
  let dec =
    let open DecodeOperation in
    d_uint32 >>= fun version ->
    if version <> !%% 0x00010000L then
      err @@ UnknownTableVersion(version)
    else
      d_uint32     >>= fun font_revision ->
      d_skip 8     >>= fun () -> (* Skips `checkSumAdjustement` and `magicNumber`. *)
      d_uint16     >>= fun flags ->
      d_uint16     >>= fun units_per_em ->
      d_timestamp  >>= fun created ->
      d_timestamp  >>= fun modified ->
      d_int16      >>= fun xmin ->
      d_int16      >>= fun ymin ->
      d_int16      >>= fun xmax ->
      d_int16      >>= fun ymax ->
      d_uint16     >>= fun mac_style ->
      d_uint16     >>= fun lowest_rec_ppem ->
      d_skip 2     >>= fun () -> (* Skips `fontDirectionHint`. *)
      d_loc_format >>= fun index_to_loc_format ->
      return Value.Head.{
        font_revision;
        flags;
        units_per_em;
        created;
        modified;
        xmin;
        ymin;
        xmax;
        ymax;
        mac_style;
        lowest_rec_ppem;
        index_to_loc_format;
      }
  in
  DecodeOperation.run common.core offset dec


let hhea (common : common_source) : Value.Hhea.t ok =
  let open ResultMonad in
  seek_required_table common Value.Tag.table_hhea >>= fun (offset, _length) ->
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
      return Value.Hhea.{
        ascender;
        descender;
        line_gap;
        advance_width_max;
        min_left_side_bearing;
        min_right_side_bearing;
        xmax_extent;
        caret_slope_rise;
        caret_slope_run;
        caret_offset;
      }
  in
  DecodeOperation.run common.core offset dec


let os2 (common : common_source) : Value.Os2.t ok =
  let open ResultMonad in
  seek_required_table common Value.Tag.table_os2 >>= fun (offset, _length) ->
  let dec =
    let open DecodeOperation in
    d_uint16 >>= fun version ->
    if 0x0005 < version then
      err @@ UnknownTableVersion(!% version)
    else
    let opt v d =
      if version < v then
        return None
      else
        d >>= fun v ->
        return (Some(v))
    in
    d_int16    >>= fun x_avg_char_width ->
    d_uint16   >>= fun us_weight_class ->
    d_uint16   >>= fun us_width_class ->
    d_uint16   >>= fun fs_type ->
    d_int16    >>= fun y_subscript_x_size ->
    d_int16    >>= fun y_subscript_y_size ->
    d_int16    >>= fun y_subscript_x_offset ->
    d_int16    >>= fun y_subscript_y_offset ->
    d_int16    >>= fun y_superscript_x_size ->
    d_int16    >>= fun y_superscript_y_size ->
    d_int16    >>= fun y_superscript_x_offset ->
    d_int16    >>= fun y_superscript_y_offset ->
    d_int16    >>= fun y_strikeout_size ->
    d_int16    >>= fun y_strikeout_position ->
    d_int16    >>= fun s_family_class ->
    d_bytes 10 >>= fun panose ->
    d_uint32   >>= fun ul_unicode_range1 ->
    d_uint32   >>= fun ul_unicode_range2 ->
    d_uint32   >>= fun ul_unicode_range3 ->
    d_uint32   >>= fun ul_unicode_range4 ->
    d_bytes 4  >>= fun ach_vend_id ->
    d_uint16   >>= fun fs_selection ->
    d_uint16   >>= fun us_first_char_index ->
    d_uint16   >>= fun us_last_char_index ->
    d_int16    >>= fun s_typo_ascender ->
    d_int16    >>= fun s_type_descender ->
    d_int16    >>= fun s_typo_linegap ->
    d_uint16   >>= fun us_win_ascent ->
    d_uint16   >>= fun us_win_descent ->
    opt 0x0001 d_uint32 >>= fun ul_code_page_range_1 ->
    opt 0x0001 d_uint32 >>= fun ul_code_page_range_2 ->
    opt 0x0002 d_int16  >>= fun s_x_height ->
    opt 0x0002 d_int16  >>= fun s_cap_height ->
    opt 0x0002 d_uint16 >>= fun us_default_char ->
    opt 0x0002 d_uint16 >>= fun us_break_char ->
    opt 0x0002 d_uint16 >>= fun us_max_context ->
    opt 0x0005 d_uint16 >>= fun us_lower_optical_point_size ->
    opt 0x0005 d_uint16 >>= fun us_upper_optical_point_size ->
    return Value.Os2.{
      x_avg_char_width;
      us_weight_class;
      us_width_class;
      fs_type;
      y_subscript_x_size;
      y_subscript_y_size;
      y_subscript_x_offset;
      y_subscript_y_offset;
      y_superscript_x_size;
      y_superscript_y_size;
      y_superscript_x_offset;
      y_superscript_y_offset;
      y_strikeout_size;
      y_strikeout_position;
      s_family_class;
      panose;
      ul_unicode_range1;
      ul_unicode_range2;
      ul_unicode_range3;
      ul_unicode_range4;
      ach_vend_id;
      fs_selection;
      us_first_char_index;
      us_last_char_index;
      s_typo_ascender;
      s_type_descender;
      s_typo_linegap;
      us_win_ascent;
      us_win_descent;
      ul_code_page_range_1;
      ul_code_page_range_2;
      s_x_height;
      s_cap_height;
      us_default_char;
      us_break_char;
      us_max_context;
      us_lower_optical_point_size;
      us_upper_optical_point_size;
    }
  in
  DecodeOperation.run common.core offset dec


let maxp (common : common_source) : Value.Maxp.t ok =
  let open ResultMonad in
  seek_required_table common Value.Tag.table_maxp >>= fun (offset, _length) ->
  let dec =
    let open DecodeOperation in
    d_uint32 >>= fun version ->
    if version = !%% 0x00005000L then
    (* If the font is based on CFF *)
      d_uint16 >>= fun num_glyphs ->
      return Value.Maxp.{
        num_glyphs               = num_glyphs;
        max_points               = 0;
        max_contours             = 0;
        max_composite_points     = 0;
        max_composite_contours   = 0;
        max_zones                = 0;
        max_twilight_points      = 0;
        max_storage              = 0;
        max_function_defs        = 0;
        max_instruction_defs     = 0;
        max_stack_elements       = 0;
        max_size_of_instructions = 0;
        max_component_elements   = 0;
        max_component_depth      = 0;
      }
    else if version = !%% 0x00010000L then
    (* If the font is based on TTF *)
      d_uint16 >>= fun num_glyphs ->
      d_uint16 >>= fun max_points ->
      d_uint16 >>= fun max_contours ->
      d_uint16 >>= fun max_composite_points ->
      d_uint16 >>= fun max_composite_contours ->
      d_uint16 >>= fun max_zones ->
      d_uint16 >>= fun max_twilight_points ->
      d_uint16 >>= fun max_storage ->
      d_uint16 >>= fun max_function_defs ->
      d_uint16 >>= fun max_instruction_defs ->
      d_uint16 >>= fun max_stack_elements ->
      d_uint16 >>= fun max_size_of_instructions ->
      d_uint16 >>= fun max_component_elements ->
      d_uint16 >>= fun max_component_depth ->
      return Value.Maxp.{
        num_glyphs;
        max_points;
        max_contours;
        max_composite_points;
        max_composite_contours;
        max_zones;
        max_twilight_points;
        max_storage;
        max_function_defs;
        max_instruction_defs;
        max_stack_elements;
        max_size_of_instructions;
        max_component_elements;
        max_component_depth;
      }
    else
      err @@ Error.UnknownTableVersion(version)
  in
  DecodeOperation.run common.core offset dec
