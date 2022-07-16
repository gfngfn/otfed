
open! Otfed__Basic
module Value = Otfed__Value


let marshaled =
  TestUtil.make_string_even [
    (* `lmroman10-regular.otf` (offset: 0x1389, length: 0x20) *)
    0x0003; 0x0000; 0x0000; 0x0000; 0xff82; 0x0028; 0x0000; 0x0000;
    0x0000; 0x0000; 0x0000; 0x0000; 0x0000; 0x0000; 0x0000; 0x0000;
  ]


let unmarshaled =
  Value.Post.{
    value = {
      italic_angle        = 0;
      underline_position  = -126;
      underline_thickness = 40;
      is_fixed_pitch      = false;
      min_mem_type_42     = 0;
      max_mem_type_42     = 0;
      min_mem_type_1      = 0;
      max_mem_type_1      = 0;
    };
    glyph_names = None;
  }
