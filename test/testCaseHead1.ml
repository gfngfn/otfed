
open Otfed__Basic
module Value = Otfed__Value
module Intermediate = Otfed__Intermediate


let marshaled =
  TestUtil.make_string_even [
    (* `ipaexm.ttf` (offset: 0x73ed24, length: 0x36) without `checkSumAdjustment` set tp 0 *)
    0x0001; 0x0000; 0x0004; 0x028f; 0x0000; 0x0000; 0x5f0f; 0x3cf5;
    0x000f; 0x0800; 0x0000; 0x0000; 0xc80e; 0x8cd9; 0x0000; 0x0000;
    0xd8e6; 0xe4bd; 0xfde5; 0xfdc5; 0x0800; 0x0754; 0x0000; 0x0006;
    0x0002; 0x0001; 0x0000;
  ]


let unmarshaled =
  Intermediate.Head.{
    value = Value.Head.{
      font_revision = WideInt.of_int 0x4028F;
      flags         = 15;
      units_per_em  = 2048;
      created       = WideInt.of_int 0xC80E8CD9;
      modified      = WideInt.of_int 0xD8E6E4BD;

      mac_style = {
        bold      = false;
        italic    = false;
        underline = false;
        outline   = false;
        shadow    = false;
        condensed = false;
        extended  = false;
      };

      lowest_rec_ppem = 6;
    };
    derived = {
      x_min               = -539;
      y_min               = -571;
      x_max               = 2048;
      y_max               = 1876;
      index_to_loc_format = Intermediate.LongLocFormat;
    };
  }
