
open! Otfed__Basic
module Value = Otfed__Value
module Intermediate = Otfed__Intermediate


let marshaled =
  TestUtil.make_string_even [
    0x0001; 0x0000; 0x070a; 0xff0a; 0x0000; 0x085a; 0xfde5; 0xfde2;
    0x0800; 0x0001; 0x0000; 0x0000; 0x0000; 0x0000; 0x0000; 0x0000;
    0x0000; 0x2fba;
  ]


let number_of_h_metrics = 12218


let unmarshaled =
  Intermediate.Hhea.{
    value = Value.Hhea.{
      ascender         = 1802;
      descender        = -246;
      line_gap         = 0;
      caret_slope_rise = 1;
      caret_slope_run  = 0;
      caret_offset     = 0;
    };
    derived = Intermediate.Hhea.{
      advance_width_max      = 2138;
      min_left_side_bearing  = -539;
      min_right_side_bearing = -542;
      xmax_extent            = 2048;
    };
  }
