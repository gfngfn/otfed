
open! Otfed__Basic
module Intermediate = Otfed__Intermediate


let marshaled =
  TestUtil.make_string_even [
    (* `lmroman10-regular.otf` (offset: 0x118, length: 0x6) *)
    0x0000; 0x5000; 0x0335;
  ]


let unmarshaled =
  Intermediate.Cff.Maxp.{
    num_glyphs = 821;
  }
