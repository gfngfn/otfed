
open! Otfed__Basic
module Value = Otfed__Value


let marshaled =
  TestUtil.make_string_even [
    (* `ipaexm.ttf` (offset 0x7574bc -l 512)  *)
    0x0002; 0x0000; 0x0000; 0x0000; 0xff0a; 0x0068; 0x0000; 0x0000;
    0x0000; 0x0000; 0x0000; 0x0000; 0x0000; 0x0000; 0x0000; 0x0000;

    0x000f; 0x0000; 0x0102; 0x0103; 0x0104; 0x0105; 0x0106; 0x0107;
    0x0108; 0x0109; 0x010a; 0x010b; 0x010c; 0x010d; 0x010e; 0x010f;

    0x052f; 0x6e75; 0x6c6c; 0x0243; 0x5203; 0x616a; 0x3103; 0x616a; (* ./null.CR.aj1.aj *)
    0x3203; 0x616a; 0x3303; 0x616a; 0x3403; 0x616a; 0x3503; 0x616a; (* 2.aj3.aj4.aj5.aj *)
    0x3603; 0x616a; 0x3703; 0x616a; 0x3803; 0x616a; 0x3904; 0x616a; (* 6.aj7.aj8.aj9.aj *)
    0x3130; 0x0461; 0x6a31; 0x3104; 0x616a; 0x3132;                 (* 10.aj11.aj12     *)
  ]


let num_glyphs = 15


let unmarshaled =
  let glyph_names =
    Value.Post.GlyphNameArray.of_list [
      ".notdef"; "/null"; "CR"; "aj1"; "aj2"; "aj3"; "aj4"; "aj5";
      "aj6"; "aj7"; "aj8"; "aj9"; "aj10"; "aj11"; "aj12";
    ]
  in
  Value.Post.{
    value = {
      italic_angle        = 0;
      underline_position  = -246;
      underline_thickness = 104;
      is_fixed_pitch      = false;
      min_mem_type_42     = 0;
      max_mem_type_42     = 0;
      min_mem_type_1      = 0;
      max_mem_type_1      = 0;
    };
    glyph_names = Some(glyph_names);
  }
