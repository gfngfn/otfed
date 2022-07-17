
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
