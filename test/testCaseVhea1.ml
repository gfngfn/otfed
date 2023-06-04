
open! Otfed__Basic
module Value = Otfed__Value
module Intermediate = Otfed__Intermediate


let marshaled =
  TestUtil.make_string_even [
    (* `ipaexm.ttf` (offset: 0x773054, length: 0x24) *)
    0x0001; 0x0000; 0x070a; 0x00f6; 0x0000; 0x0800; 0xffb6; 0xfebb;
    0x0945; 0x0000; 0x0001; 0x0000; 0x0000; 0x0000; 0x0000; 0x0000;
    0x0000; 0x0001;
  ]


let number_of_long_ver_metrics = 1


let unmarshaled =
  Intermediate.Vhea.{
    value = Value.Vhea.{
      metrics          = Version1_0 { ascent = 1802; descent = 246 };
      caret_slope_rise = 0;
      caret_slope_run  = 1;
      caret_offset     = 0;
    };
    derived = {
      advance_height_max      = 2048;
      min_top_side_bearing    = -74;
      min_bottom_side_bearing = -325;
      y_max_extent            = 2373;
    };
  }
