
open Basic
open Value
open DecodeBasic
open DecodeOperation.Open


let d_math_value_record (offset_origin : offset) : Math.math_value_record decoder =
  let open DecodeOperation in
  d_int16 >>= fun value ->
  d_fetch_opt offset_origin d_device >>= fun device_table_opt ->
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


let get (src : source) : (Math.t option) ok =
  let open ResultMonad in
  let common = get_common_source src in
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
