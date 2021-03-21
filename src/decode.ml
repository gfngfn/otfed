
open Basic
open Value
open DecodeOperation.Open


include DecodeBasic


let fetch_loc_format core table_directory =
  let open ResultMonad in
  DecodeOperation.seek_required_table table_directory Tag.table_head >>= fun (offset, _length) ->
  let open DecodeOperation in
  d_loc_format |> run core (offset + 50)


let fetch_num_glyphs core table_directory =
  let open ResultMonad in
  DecodeOperation.seek_required_table table_directory Tag.table_maxp >>= fun (offset, _length) ->
  let open DecodeOperation in
  d_uint16 |> run core (offset + 4)


let fetch_num_h_metrics core table_directory =
  let open ResultMonad in
  DecodeOperation.seek_required_table table_directory Tag.table_hhea >>= fun (offset, _length) ->
  let open DecodeOperation in
  d_uint16 |> run core (offset + 34)


let d_init_ttf core =
  let open DecodeOperation in
  d_structure >>= fun table_directory ->
  transform_result (fetch_loc_format core table_directory) >>= fun loc_format ->
  transform_result (fetch_num_glyphs core table_directory) >>= fun num_glyphs ->
  transform_result (fetch_num_h_metrics core table_directory) >>= fun num_h_metrics ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
      loc_format      = loc_format;
      num_glyphs      = num_glyphs;
      num_h_metrics   = num_h_metrics;
    }
  in
  let ttf = {ttf_common = common} in
  return @@ (common, Ttf(ttf))


let d_init_cff (core : common_source_core) : source decoder =
  let open DecodeOperation in
  d_structure >>= fun table_directory ->
  transform_result (fetch_loc_format core table_directory) >>= fun loc_format ->
  transform_result (fetch_num_glyphs core table_directory) >>= fun num_glyphs ->
  transform_result (fetch_num_h_metrics core table_directory) >>= fun num_h_metrics ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
      loc_format      = loc_format;
      num_glyphs      = num_glyphs;
      num_h_metrics   = num_h_metrics;
    }
  in
  let cff = {cff_common = common} in
  return @@ (common, Cff(cff))


let source_of_string (s : string) : single_or_collection ok =
  let core =
    {
      data = s;
      max  = String.length s;
    }
  in
  let dec =
    let open DecodeOperation in
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
  DecodeOperation.run core 0 dec


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
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_cmap >>= fun (offset, length) ->
  return @@ Intermediate.Cmap.make common.core ~offset ~length


let head (common : common_source) : Value.Head.t ok =
  let open ResultMonad in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_head >>= fun (offset, _length) ->
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
      (* Skips `fontDirectionHint` and `indexToLocFormat`. *)
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
      }
  in
  DecodeOperation.run common.core offset dec


let hhea (common : common_source) : Value.Hhea.t ok =
  let open ResultMonad in
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
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_os2 >>= fun (offset, _length) ->
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
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_maxp >>= fun (offset, _length) ->
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


let hmtx (common : common_source) =
  let open ResultMonad in
  DecodeOperation.seek_required_table common.table_directory Tag.table_hmtx >>= fun (offset, length) ->
  let num_glyphs = common.num_glyphs in
  let num_h_metrics = common.num_h_metrics in
  return @@ DecodeIntermediate.Hmtx.make common.core ~offset ~length ~num_glyphs ~num_h_metrics


let gsub (common : common_source) =
  let open ResultMonad in
  match DecodeOperation.seek_table common.table_directory Tag.table_gsub with
  | None ->
      return None

  | Some((offset, length)) ->
      return @@ Some(DecodeIntermediate.Gsub.make common.core ~offset ~length)


let gpos (common : common_source) =
  let open ResultMonad in
  match DecodeOperation.seek_table common.table_directory Tag.table_gpos with
  | None ->
      return None

  | Some((offset, length)) ->
      return @@ Some(DecodeIntermediate.Gpos.make common.core ~offset ~length)


let d_device_table : Math.device_table decoder =
  let open DecodeOperation in
  d_uint16 >>= fun startSize ->
  d_uint16 >>= fun endSize ->
  d_uint16 >>= fun deltaFormat ->
  d_uint16 >>= fun deltaValue ->
  return (startSize, endSize, deltaFormat, deltaValue)


let d_math_value_record (offset_origin : offset) : Math.math_value_record decoder =
  let open DecodeOperation in
  d_int16 >>= fun value ->
  d_fetch_opt offset_origin d_device_table >>= fun device_table_opt ->
  return (value, device_table_opt)


let d_math_constants : Math.math_constants decoder =
  let open DecodeOperation in
  current >>= fun offset_origin ->
  let dm = d_math_value_record offset_origin in
  d_int16  >>= fun script_percent_scale_down ->
  d_int16  >>= fun script_script_percent_scale_down ->
  d_uint16 >>= fun delimited_sub_formula_min_height ->
  d_uint16 >>= fun display_operator_min_height ->
  dm       >>= fun math_leading ->
  dm       >>= fun axis_height ->
  dm       >>= fun accent_base_height ->
  dm       >>= fun flattened_accent_base_height ->
  dm       >>= fun subscript_shift_down ->
  dm       >>= fun subscript_top_max ->
  dm       >>= fun subscript_baseline_drop_min ->
  dm       >>= fun superscript_shift_up ->
  dm       >>= fun superscript_shift_up_cramped ->
  dm       >>= fun superscript_bottom_min ->
  dm       >>= fun superscript_baseline_drop_max ->
  dm       >>= fun sub_superscript_gap_min ->
  dm       >>= fun superscript_bottom_max_with_subscript ->
  dm       >>= fun space_after_script ->
  dm       >>= fun upper_limit_gap_min ->
  dm       >>= fun upper_limit_baseline_rise_min ->
  dm       >>= fun lower_limit_gap_min ->
  dm       >>= fun lower_limit_baseline_drop_min ->
  dm       >>= fun stack_top_shift_up ->
  dm       >>= fun stack_top_display_style_shift_up ->
  dm       >>= fun stack_bottom_shift_down ->
  dm       >>= fun stack_bottom_display_style_shift_down ->
  dm       >>= fun stack_gap_min ->
  dm       >>= fun stack_display_style_gap_min ->
  dm       >>= fun stretch_stack_top_shift_up ->
  dm       >>= fun stretch_stack_bottom_shift_down ->
  dm       >>= fun stretch_stack_gap_above_min ->
  dm       >>= fun stretch_stack_gap_below_min ->
  dm       >>= fun fraction_numerator_shift_up ->
  dm       >>= fun fraction_numerator_display_style_shift_up ->
  dm       >>= fun fraction_denominator_shift_down ->
  dm       >>= fun fraction_denominator_display_style_shift_down ->
  dm       >>= fun fraction_numerator_gap_min ->
  dm       >>= fun fraction_num_display_style_gap_min ->
  dm       >>= fun fraction_rule_thickness ->
  dm       >>= fun fraction_denominator_gap_min ->
  dm       >>= fun fraction_denom_display_style_gap_min ->
  dm       >>= fun skewed_fraction_horizontal_gap ->
  dm       >>= fun skewed_fraction_vertical_gap ->
  dm       >>= fun overbar_vertical_gap ->
  dm       >>= fun overbar_rule_thickness ->
  dm       >>= fun overbar_extra_ascender ->
  dm       >>= fun underbar_vertical_gap ->
  dm       >>= fun underbar_rule_thickness ->
  dm       >>= fun underbar_extra_descender ->
  dm       >>= fun radical_vertical_gap ->
  dm       >>= fun radical_display_style_vertical_gap ->
  dm       >>= fun radical_rule_thickness ->
  dm       >>= fun radical_extra_ascender ->
  dm       >>= fun radical_kern_before_degree ->
  dm       >>= fun radical_kern_after_degree ->
  d_int16  >>= fun radical_degree_bottom_raise_percent ->
  return Math.{
    script_percent_scale_down;
    script_script_percent_scale_down;
    delimited_sub_formula_min_height;
    display_operator_min_height;
    math_leading;
    axis_height;
    accent_base_height;
    flattened_accent_base_height;
    subscript_shift_down;
    subscript_top_max;
    subscript_baseline_drop_min;
    superscript_shift_up;
    superscript_shift_up_cramped;
    superscript_bottom_min;
    superscript_baseline_drop_max;
    sub_superscript_gap_min;
    superscript_bottom_max_with_subscript;
    space_after_script;
    upper_limit_gap_min;
    upper_limit_baseline_rise_min;
    lower_limit_gap_min;
    lower_limit_baseline_drop_min;
    stack_top_shift_up;
    stack_top_display_style_shift_up;
    stack_bottom_shift_down;
    stack_bottom_display_style_shift_down;
    stack_gap_min;
    stack_display_style_gap_min;
    stretch_stack_top_shift_up;
    stretch_stack_bottom_shift_down;
    stretch_stack_gap_above_min;
    stretch_stack_gap_below_min;
    fraction_numerator_shift_up;
    fraction_numerator_display_style_shift_up;
    fraction_denominator_shift_down;
    fraction_denominator_display_style_shift_down;
    fraction_numerator_gap_min;
    fraction_num_display_style_gap_min;
    fraction_rule_thickness;
    fraction_denominator_gap_min;
    fraction_denom_display_style_gap_min;
    skewed_fraction_horizontal_gap;
    skewed_fraction_vertical_gap;
    overbar_vertical_gap;
    overbar_rule_thickness;
    overbar_extra_ascender;
    underbar_vertical_gap;
    underbar_rule_thickness;
    underbar_extra_descender;
    radical_vertical_gap;
    radical_display_style_vertical_gap;
    radical_rule_thickness;
    radical_extra_ascender;
    radical_kern_before_degree;
    radical_kern_after_degree;
    radical_degree_bottom_raise_percent;
  }


let d_math_italics_correction_info : ((glyph_id * Math.math_value_record) list) decoder =
  let open DecodeOperation in
  current >>= fun offset_MathItalicCollectionInfo ->
  d_fetch offset_MathItalicCollectionInfo d_coverage >>= fun coverage ->
  d_list (d_math_value_record offset_MathItalicCollectionInfo) >>= fun vs ->
  combine_coverage coverage vs


let d_math_top_accent_attachment : ((glyph_id * Math.math_value_record) list) decoder =
  let open DecodeOperation in
  current >>= fun offset_MathTopAccentAttachment ->
  d_fetch offset_MathTopAccentAttachment d_coverage >>= fun coverage ->
  d_list (d_math_value_record offset_MathTopAccentAttachment) >>= fun vs ->
  combine_coverage coverage vs


let d_math_kern : Math.math_kern decoder =
  let open DecodeOperation in
  current >>= fun offset_MathKern ->
  d_uint16 >>= fun heightCount ->
  d_repeat heightCount (d_math_value_record offset_MathKern) >>= fun correctionHeights ->
  d_repeat (heightCount + 1) (d_math_value_record offset_MathKern) >>= fun kernValues ->
  return (correctionHeights, kernValues)


let d_math_kern_info_record (offset_MathKernInfo : offset) : Math.math_kern_info_record decoder =
  let open DecodeOperation in
  d_fetch_opt offset_MathKernInfo d_math_kern >>= fun top_right_math_kern ->
  d_fetch_opt offset_MathKernInfo d_math_kern >>= fun top_left_math_kern ->
  d_fetch_opt offset_MathKernInfo d_math_kern >>= fun bottom_right_math_kern ->
  d_fetch_opt offset_MathKernInfo d_math_kern >>= fun bottom_left_math_kern ->
  return Math.{
    top_right_math_kern;
    top_left_math_kern;
    bottom_right_math_kern;
    bottom_left_math_kern;
  }


let d_math_kern_info : ((glyph_id * Math.math_kern_info_record) list) decoder =
  let open DecodeOperation in
  current >>= fun offset_MathKernInfo ->
  d_fetch offset_MathKernInfo d_coverage >>= fun coverage ->
  d_list (d_math_kern_info_record offset_MathKernInfo) >>= fun vs ->
  combine_coverage coverage vs


let d_math_glyph_info : Math.math_glyph_info decoder =
  let open DecodeOperation in
  current >>= fun offset_MathGlyphInfo ->
  d_fetch offset_MathGlyphInfo d_math_italics_correction_info >>= fun math_italics_correction ->
  d_fetch offset_MathGlyphInfo d_math_top_accent_attachment >>= fun math_top_accent_attachment ->
  d_fetch_opt offset_MathGlyphInfo d_coverage >>= fun _ ->
  d_fetch_opt offset_MathGlyphInfo d_math_kern_info >>= fun math_kern_info_opt ->
  begin
    match math_kern_info_opt with
    | None ->
        return []

    | Some(math_kern_info) ->
        return math_kern_info
  end >>= fun math_kern_info ->
  return Math.{
    math_italics_correction;
    math_top_accent_attachment;
    math_kern_info;
  }


let d_glyph_part_record : Math.glyph_part_record decoder =
  let open DecodeOperation in
  d_uint16 >>= fun glyph_id_for_part ->
  d_uint16 >>= fun start_connector_length ->
  d_uint16 >>= fun end_connector_length ->
  d_uint16 >>= fun full_advance ->
  d_uint16 >>= fun part_flags ->
  return Math.{
    glyph_id_for_part;
    start_connector_length;
    end_connector_length;
    full_advance;
    part_flags;
  }


let d_glyph_assembly : (Math.math_value_record * Math.glyph_part_record list) decoder =
  let open DecodeOperation in
  current >>= fun offset_GlyphAssembly ->
  d_math_value_record offset_GlyphAssembly >>= fun italicsCorrection ->
  d_list d_glyph_part_record >>= fun partRecords ->
  return (italicsCorrection, partRecords)


let d_math_glyph_variant_record : (glyph_id * int) decoder =
  let open DecodeOperation in
  d_uint16 >>= fun variantGlyph ->
  d_uint16 >>= fun advanceMeasurement ->
  return (variantGlyph, advanceMeasurement)


let d_math_glyph_construction : Math.math_glyph_construction decoder =
  let open DecodeOperation in
  current >>= fun offset_MathGlyphConstruction ->
  d_fetch_opt offset_MathGlyphConstruction d_glyph_assembly >>= fun glyph_assembly ->
  d_list d_math_glyph_variant_record >>= fun math_glyph_variant_record_list ->
  return Math.{
    glyph_assembly;
    math_glyph_variant_record_list;
  }


let d_math_variants : Math.math_variants decoder =
  let open DecodeOperation in
  current >>= fun offset_MathVariants ->
  d_uint16 >>= fun min_connector_overlap ->
  d_fetch offset_MathVariants d_coverage >>= fun vertGlyphCoverage ->
  d_fetch offset_MathVariants d_coverage >>= fun horizGlyphCoverage ->
  d_uint16 >>= fun vertGlyphCount ->
  d_uint16 >>= fun horizGlyphCount ->
  let df = d_fetch offset_MathVariants d_math_glyph_construction in
  d_repeat vertGlyphCount df >>= fun vertGlyphConstructions ->
  d_repeat horizGlyphCount df >>= fun horizGlyphConstructions ->
  combine_coverage vertGlyphCoverage vertGlyphConstructions >>= fun vert_glyph_assoc ->
  combine_coverage horizGlyphCoverage horizGlyphConstructions >>= fun horiz_glyph_assoc ->
  return Math.{
    min_connector_overlap;
    vert_glyph_assoc;
    horiz_glyph_assoc;
  }


let math (common : common_source) =
  let open ResultMonad in
  match DecodeOperation.seek_table common.table_directory Tag.table_math with
  | None ->
      return None

  | Some((offset, _length)) ->
      let dec =
        let open DecodeOperation in
        d_uint32 >>= fun version ->
        if version <> !%% 0x00010000L then
          err @@ Error.UnknownTableVersion(version)
        else
          d_fetch offset d_math_constants  >>= fun math_constants ->
          d_fetch offset d_math_glyph_info >>= fun math_glyph_info ->
          d_fetch offset d_math_variants   >>= fun math_variants ->
          return Math.{
            math_constants;
            math_glyph_info;
            math_variants;
          }
      in
      dec |> DecodeOperation.run common.core offset >>= fun math ->
      return @@ Some(math)


include DecodeTtf

include DecodeCff

module ForTest = struct
  type charstring_data = DecodeBasic.charstring_data
  type subroutine_index = DecodeBasic.subroutine_index
  type 'a decoder = 'a DecodeOperation.Open.decoder

  let run s d =
    DecodeOperation.run { data = s; max = String.length s } 0 d

  let d_glyf =
    d_glyf

  let run_d_charstring ~gsubr_index ~lsubr_index data ~start ~charstring_length =
    let cstate = initial_charstring_state charstring_length in
    let dec =
      let open DecodeOperation in
      d_charstring { gsubr_index; lsubr_index } cstate >>= fun (_, opacc) ->
      return @@ Alist.to_list opacc
    in
    dec |> DecodeOperation.run { data = data; max = String.length data } start
end
