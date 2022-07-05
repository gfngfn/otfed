
open! Otfed__Basic
module Intermediate = Otfed__Intermediate


let marshaled =
  TestUtil.make_string_even [
    (* `ipaexm.ttf` (offset: 0x756bd4, length: 0x20) *)
    0x0001; 0x0000; 0x2fcf; 0x0146; 0x0031; 0x0000; 0x0000; 0x0002;
    0x0008; 0x0000; 0x0024; 0x0000; 0x1000; 0x0345; 0x0000; 0x0000;
  ]


let unmarshaled =
  Intermediate.Ttf.Maxp.{
    num_glyphs               = 12239;
    max_points               = 326;
    max_contours             = 49;
    max_composite_points     = 0;
    max_composite_contours   = 0;
    max_zones                = 2;
    max_twilight_points      = 8;
    max_storage              = 0;
    max_function_defs        = 36;
    max_instruction_defs     = 0;
    max_stack_elements       = 4096;
    max_size_of_instructions = 837;
    max_component_elements   = 0;
    max_component_depth      = 0;
  }
