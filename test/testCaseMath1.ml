
open! Otfed__Basic
module Value = Otfed__Value


let marshaled_constants =
  TestUtil.make_string_even [
    (* `latinmodern-math.otf` (offset: 689258, length: 214) *)
    0x0046; 0x0032; 0x0514; 0x0514; 0x009a; 0x0000; 0x00fa; 0x0000;
    0x01c2; 0x0000; 0x0298; 0x0000; 0x00f7; 0x0000; 0x0158; 0x0000;
    0x00c8; 0x0000; 0x016b; 0x0000; 0x0121; 0x0000; 0x006c; 0x0000;
    0x00fa; 0x0000; 0x00a0; 0x0000; 0x0158; 0x0000; 0x0038; 0x0000;
    0x00c8; 0x0000; 0x006f; 0x0000; 0x00a7; 0x0000; 0x0258; 0x0000;
    0x01bc; 0x0000; 0x02a5; 0x0000; 0x0159; 0x0000; 0x02ae; 0x0000;
    0x0078; 0x0000; 0x0118; 0x0000; 0x006f; 0x0000; 0x0258; 0x0000;
    0x00c8; 0x0000; 0x00a7; 0x0000; 0x018a; 0x0000; 0x02a5; 0x0000;
    0x0159; 0x0000; 0x02ae; 0x0000; 0x0028; 0x0000; 0x0078; 0x0000;
    0x0028; 0x0000; 0x0028; 0x0000; 0x0078; 0x0000; 0x015e; 0x0000;
    0x0060; 0x0000; 0x0078; 0x0000; 0x0028; 0x0000; 0x0028; 0x0000;
    0x0078; 0x0000; 0x0028; 0x0000; 0x0028; 0x0000; 0x0032; 0x0000;
    0x0094; 0x0000; 0x0028; 0x0000; 0x0028; 0x0000; 0x0116; 0x0000;
    0xfdd4; 0x0000; 0x003c;
  ]


let unmarshaled_constants =
   Value.Math.{
     script_percent_scale_down                     = 70;
     script_script_percent_scale_down              = 50;
     delimited_sub_formula_min_height              = 1300;
     display_operator_min_height                   = 1300;
     math_leading                                  = (154, None);
     axis_height                                   = (250, None);
     accent_base_height                            = (450, None);
     flattened_accent_base_height                  = (664, None);
     subscript_shift_down                          = (247, None);
     subscript_top_max                             = (344, None);
     subscript_baseline_drop_min                   = (200, None);
     superscript_shift_up                          = (363, None);
     superscript_shift_up_cramped                  = (289, None);
     superscript_bottom_min                        = (108, None);
     superscript_baseline_drop_max                 = (250, None);
     sub_superscript_gap_min                       = (160, None);
     superscript_bottom_max_with_subscript         = (344, None);
     space_after_script                            = (56, None);
     upper_limit_gap_min                           = (200, None);
     upper_limit_baseline_rise_min                 = (111, None);
     lower_limit_gap_min                           = (167, None);
     lower_limit_baseline_drop_min                 = (600, None);
     stack_top_shift_up                            = (444, None);
     stack_top_display_style_shift_up              = (677, None);
     stack_bottom_shift_down                       = (345, None);
     stack_bottom_display_style_shift_down         = (686, None);
     stack_gap_min                                 = (120, None);
     stack_display_style_gap_min                   = (280, None);
     stretch_stack_top_shift_up                    = (111, None);
     stretch_stack_bottom_shift_down               = (600, None);
     stretch_stack_gap_above_min                   = (200, None);
     stretch_stack_gap_below_min                   = (167, None);
     fraction_numerator_shift_up                   = (394, None);
     fraction_numerator_display_style_shift_up     = (677, None);
     fraction_denominator_shift_down               = (345, None);
     fraction_denominator_display_style_shift_down = (686, None);
     fraction_numerator_gap_min                    = (40, None);
     fraction_num_display_style_gap_min            = (120, None);
     fraction_rule_thickness                       = (40, None);
     fraction_denominator_gap_min                  = (40, None);
     fraction_denom_display_style_gap_min          = (120, None);
     skewed_fraction_horizontal_gap                = (350, None);
     skewed_fraction_vertical_gap                  = (96, None);
     overbar_vertical_gap                          = (120, None);
     overbar_rule_thickness                        = (40, None);
     overbar_extra_ascender                        = (40, None);
     underbar_vertical_gap                         = (120, None);
     underbar_rule_thickness                       = (40, None);
     underbar_extra_descender                      = (40, None);
     radical_vertical_gap                          = (50, None);
     radical_display_style_vertical_gap            = (148, None);
     radical_rule_thickness                        = (40, None);
     radical_extra_ascender                        = (40, None);
     radical_kern_before_degree                    = (278, None);
     radical_kern_after_degree                     = (-556, None);
     radical_degree_bottom_raise_percent           = 60;
   }


let marshaled_italics_correction_info =
  let marshaled_coverage =
    TestUtil.make_string_even [
      (* `CoverageFormat` and `GlyphCount` *)
      0x0001; 0x0014;

      (* `latinmodern-math.otf` (offset: 693492) truncated to the first 20 entries *)
      0x0018; 0x0033; 0x0037; 0x0038; 0x0039; 0x003a; 0x003c; 0x003f;
      0x0040; 0x0042; 0x0047; 0x0048; 0x0049; 0x004c; 0x004d; 0x004e;
      0x004f; 0x0052; 0x0056; 0x0057;
    ]
  in
  let marshaled_value_record_list =
    TestUtil.make_string_even [
      (* `latinmodern-math.otf` (offset: 693492) truncated to the first 20 entries *)
      0x000d; 0x0000; 0x0018; 0x0000; 0x0008; 0x0000; 0x0009; 0x0000;
      0x0004; 0x0000; 0x0010; 0x0000; 0x0006; 0x0000; 0x0021; 0x0000;
      0x001c; 0x0000; 0x000b; 0x0000; 0x004f; 0x0000; 0x000d; 0x0000;
      0x0007; 0x0000; 0x000b; 0x0000; 0x0005; 0x0000; 0x0008; 0x0000;
      0x0007; 0x0000; 0x001b; 0x0000; 0x0007; 0x0000; 0x0008; 0x0000;
    ]
  in
  let reloffset_coverage = 4 + (String.length marshaled_value_record_list) in
  let italics_correction_count = 20 in
  String.concat "" [
    TestUtil.make_string_even [ reloffset_coverage; italics_correction_count; ];
    marshaled_value_record_list;
    marshaled_coverage;
  ]


let unmarshaled_italics_correction_info =
  [
    (24, (13, None)); (51, (24, None)); (55, (8, None)); (56, (9, None));
    (57, (4, None)); (58, (16, None)); (60, (6, None)); (63, (33, None));
    (64, (28, None)); (66, (11, None)); (71, (79, None)); (72, (13, None));
    (73, (7, None)); (76, (11, None)); (77, (5, None)); (78, (8, None));
    (79, (7, None)); (82, (27, None)); (86, (7, None)); (87, (8, None));
  ]


let marshaled_variants =
  let vert_glyph_ids = [] in
  let horiz_glyph_ids = [ 0x0937 (* GID 2359 *) ] in

  let vert_glyph_count = List.length vert_glyph_ids in
  let horiz_glyph_count = List.length horiz_glyph_ids in
  let marshaled_vert_glyph_coverage =
    TestUtil.make_string_even (List.concat [
      (* `CoverageFormat` and `GlyphCount`: *)
      [ 0x0001; vert_glyph_count ];
      vert_glyph_ids;
    ])
  in
  let marshaled_horiz_glyph_coverage =
    TestUtil.make_string_even (List.concat [
      (* `CoverageFormat` and `GlyphCount`: *)
      [ 0x0001; horiz_glyph_count ];
      horiz_glyph_ids;
    ])
  in
  let marshaled_math_glyph_construction_and_glyph_assembly =
    let marshaled_math_glyph_construction =
      TestUtil.make_string_even [
        (* `GlyphAssembly` (offset): *)
        0x0024;

        (* `VariantCount`: *)
        0x0008;

        (* `MathGlyphVariantRecord[VariantCount]`: *)
        0x0937; 0x01ed;
        0x094d; 0x03e2;
        0x0963; 0x05d7;
        0x0979; 0x07cd;
        0x098f; 0x09c3;
        0x09a5; 0x0bb9;
        0x09bb; 0x0daf;
        0x09f1; 0x0fa7;
      ]
    in
    let marshaled_glyph_assembly =
      TestUtil.make_string_even [
        (* `latinmodern-math.otf` (offset: 713688, length: 56), the `GlyphAssembly` table for GID 2359 *)

        (* `ItalicsCorrection`: *)
        0x0000; 0x0000;

        (* `PartCount`: *)
        0x0005;

        (* `PartRecords[PartCount]` *)
        0x09f3; 0x0000; 0x01f1; 0x03ea; 0x0000;
        0x09f4; 0x03e2; 0x03e2; 0x03e2; 0x0001;
        0x09f5; 0x01f1; 0x01f1; 0x07d3; 0x0000;
        0x09f4; 0x03e2; 0x03e2; 0x03e2; 0x0001;
        0x09f6; 0x01f1; 0x0000; 0x03e9; 0x0000;
      ]
    in
    String.concat "" [
      marshaled_math_glyph_construction;
      marshaled_glyph_assembly;
    ]
  in
  let constant_entry_length = 10 in
  let reloffset_contents = constant_entry_length + 2 * vert_glyph_count + 2 * horiz_glyph_count in
  let marshaled_vert_glyph_construction_array =
    ""
  in
  let marshaled_horiz_glyph_construction_array =
    TestUtil.make_string_even [
      reloffset_contents; (* GID 2359 *)
    ]
  in
  let reloffset_vert_glyph_coverage =
    reloffset_contents + String.length marshaled_math_glyph_construction_and_glyph_assembly
  in
  let reloffset_horiz_glyph_coverage =
    reloffset_vert_glyph_coverage + String.length marshaled_vert_glyph_coverage
  in
  String.concat "" [
    TestUtil.make_string_even [
      (* `MinConnectorOverlap`: *)
      0x0014;

      (* `VertGlyphCoverage`: *)
      reloffset_vert_glyph_coverage;

      (* `HorizGlyphCoverage`: *)
      reloffset_horiz_glyph_coverage;

      (* `VertGlyphCount`: *)
      vert_glyph_count;

      (* `HorizGlyphCount`: *)
      horiz_glyph_count;
    ];
    (* `VertGlyphConstruction[VertGlyphCount]`: *)
    marshaled_vert_glyph_construction_array;

    (* `HorizGlyphConstruction[HorizGlyphCount]`: *)
    marshaled_horiz_glyph_construction_array;

    marshaled_math_glyph_construction_and_glyph_assembly;
    marshaled_vert_glyph_coverage;
    marshaled_horiz_glyph_coverage;
  ]


let unmarshaled_variants =
  Value.Math.{
    min_connector_overlap = 20;
    vert_glyph_assoc = [];
    horiz_glyph_assoc = [
      (* Overbrace: *)
      (2359,
       { glyph_assembly =
           Some((
             (0, None),
             [
               {
                 glyph_id_for_part      = 2547;
                 start_connector_length = 0;
                 end_connector_length   = 497;
                 full_advance           = 1002;
                 f_extender             = false;
               };
               {
                 glyph_id_for_part      = 2548;
                 start_connector_length = 994;
                 end_connector_length   = 994;
                 full_advance           = 994;
                 f_extender             = true;
               };
               { glyph_id_for_part      = 2549;
                 start_connector_length = 497;
                 end_connector_length   = 497;
                 full_advance           = 2003;
                 f_extender             = false;
               };
               {
                 glyph_id_for_part      = 2548;
                 start_connector_length = 994;
                 end_connector_length   = 994;
                 full_advance           = 994;
                 f_extender             = true;
               };
               {
                 glyph_id_for_part      = 2550;
                 start_connector_length = 497;
                 end_connector_length   = 0;
                 full_advance           = 1001;
                 f_extender             = false;
               };
             ]));
         math_glyph_variant_record_list =
           [
             { variant_glyph = 2359; advance_measurement = 493 };
             { variant_glyph = 2381; advance_measurement = 994 };
             { variant_glyph = 2403; advance_measurement = 1495 };
             { variant_glyph = 2425; advance_measurement = 1997 };
             { variant_glyph = 2447; advance_measurement = 2499 };
             { variant_glyph = 2469; advance_measurement = 3001 };
             { variant_glyph = 2491; advance_measurement = 3503 };
             { variant_glyph = 2545; advance_measurement = 4007 };
           ]
       });
    ];
  }
