
open! Otfed__Basic
module Value = Otfed__Value
module Intermediate = Otfed__Intermediate


let marshaled =
  TestUtil.make_string_even [
    0x0003; 0x03ce; 0x0190; 0x0005; 0x0000; 0x0400; 0x062b; 0x0000;
    0x0133; 0x0400; 0x062b; 0x0000; 0x0000; 0x005e; 0x035a; 0x0105;
    0x0202; 0x0400; 0x0000; 0x0000; 0x0000; 0xe000; 0x02ff; 0x3ac7;
    0xedfa; 0x0000; 0x0012; 0x0000; 0x0000; 0x4950; 0x4120; 0x0040;
    0x0020; 0xffe8; 0x070a; 0xff0a; 0x0000; 0x0754; 0x023b; 0x0002;
    0x0001; 0x0000; 0x0000; 0x03f0; 0x05b8; 0x0000; 0x0020; 0x0006;
  ]


let unmarshaled =
  Intermediate.Os2.{
    value = Value.Os2.{
      us_weight_class = WeightNormal;
      us_width_class  = WidthMedium;

      fs_type = Value.Os2.{
        restricted_license_embedding = false;
        preview_and_print_embedding  = false;
        editable_embedding           = false;
        no_subsetting                = false;
        bitmap_embedding_only        = false;
      };

      y_subscript_x_size     = 1024;
      y_subscript_y_size     = 1579;
      y_subscript_x_offset   = 0;
      y_subscript_y_offset   = 307;
      y_superscript_x_size   = 1024;
      y_superscript_y_size   = 1579;
      y_superscript_x_offset = 0;
      y_superscript_y_offset = 0;
      y_strikeout_size       = 94;
      y_strikeout_position   = 858;
      s_family_class         = 261;

      panose = TestUtil.make_string_even [ 0x0202; 0x0400; 0x0000; 0x0000; 0x0000; ];

      ul_unicode_range1 = WideInt.of_int 0xE00002FF;
      ul_unicode_range2 = WideInt.of_int 0x3AC7EDFA;
      ul_unicode_range3 = WideInt.of_int 0x12;
      ul_unicode_range4 = WideInt.of_int 0x0;

      ach_vend_id  = "IPA ";
      fs_selection = 64;

      s_typo_ascender  = 1802;
      s_typo_descender = -246;
      s_typo_linegap   = 0;
      us_win_ascent    = 1876;
      us_win_descent   = 571;

      ul_code_page_range1 = Some(WideInt.of_int 0x20001);
      ul_code_page_range2 = Some(WideInt.of_int 0x0);

      s_x_height   = Some(1008);
      s_cap_height = Some(1464);

      us_default_char = Some(Uchar.of_int 0x0000);
      us_break_char   = Some(Uchar.of_int 0x0020);

      us_lower_optical_point_size = None;
      us_upper_optical_point_size = None;
    };
    derived = Intermediate.Os2.{
      x_avg_char_width    = 974;
      us_first_char_index = Uchar.of_int 0x0020;
      us_last_char_index  = Uchar.of_int 0xFFE8;
      us_max_context      = Some(6);
    };
  }
