
module Value = Otfed__Value


let marshaled =
  TestUtil.make_string_even [
    (* `Junicode.ttf`, glyph ID 264 (offset: 100808, length: 24) without padded bytes *)
    0xffff; 0x0031; 0xfffa; 0x0342; 0x057f; 0x1027; 0x00ae; 0x01e4;
    0xffff; 0x1106; 0x0107; 0x0000;
  ]


let unmarshaled =
  Value.Ttf.{
    bounding_box = {
      x_min = 49;
      y_min = -6;
      x_max = 834;
      y_max = 1407;
    };
    description =
      CompositeGlyph([
        {
          component_glyph_id        = 174;
          composition               = Vector((484, -1));
          component_scale           = None;
          round_xy_to_grid          = true;
          use_my_metrics            = false;
          unscaled_component_offset = true;
        };
        {
          component_glyph_id        = 263;
          composition               = Vector((0, 0));
          component_scale           = None;
          round_xy_to_grid          = true;
          use_my_metrics            = false;
          unscaled_component_offset = true;
        };
      ]);
  }
