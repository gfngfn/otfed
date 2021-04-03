
open Basic
open DecodeBasic


type derived = {
  x_avg_char_width    : int;
  ul_unicode_range1   : wint;
  ul_unicode_range2   : wint;
  ul_unicode_range3   : wint;
  ul_unicode_range4   : wint;
  us_first_char_index : int;
  us_last_char_index  : int;
  us_max_context      : int option;
}
[@@deriving show { with_path = false }]

type t = {
  value   : Value.Os2.t;
  derived : derived;
}
[@@deriving show { with_path = false }]


let get (src : source) : t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_os2 >>= fun (offset, _length) ->
  let dec =
    let open DecodeOperation in
    d_uint16 >>= fun version ->
    if 0x0005 < version then
      err @@ UnknownTableVersion(!% version)
    else
    let opt v d =
      if version < v then
        return None
      else
        d >>= fun v ->
        return (Some(v))
    in
    d_int16    >>= fun x_avg_char_width ->
    d_uint16   >>= fun us_weight_class ->
    d_uint16   >>= fun us_width_class ->
    d_uint16   >>= fun fs_type ->
    d_int16    >>= fun y_subscript_x_size ->
    d_int16    >>= fun y_subscript_y_size ->
    d_int16    >>= fun y_subscript_x_offset ->
    d_int16    >>= fun y_subscript_y_offset ->
    d_int16    >>= fun y_superscript_x_size ->
    d_int16    >>= fun y_superscript_y_size ->
    d_int16    >>= fun y_superscript_x_offset ->
    d_int16    >>= fun y_superscript_y_offset ->
    d_int16    >>= fun y_strikeout_size ->
    d_int16    >>= fun y_strikeout_position ->
    d_int16    >>= fun s_family_class ->
    d_bytes 10 >>= fun panose ->
    d_uint32   >>= fun ul_unicode_range1 ->
    d_uint32   >>= fun ul_unicode_range2 ->
    d_uint32   >>= fun ul_unicode_range3 ->
    d_uint32   >>= fun ul_unicode_range4 ->
    d_bytes 4  >>= fun ach_vend_id ->
    d_uint16   >>= fun fs_selection ->
    d_uint16   >>= fun us_first_char_index ->
    d_uint16   >>= fun us_last_char_index ->
    d_int16    >>= fun s_typo_ascender ->
    d_int16    >>= fun s_type_descender ->
    d_int16    >>= fun s_typo_linegap ->
    d_uint16   >>= fun us_win_ascent ->
    d_uint16   >>= fun us_win_descent ->
    opt 0x0001 d_uint32 >>= fun ul_code_page_range_1 ->
    opt 0x0001 d_uint32 >>= fun ul_code_page_range_2 ->
    opt 0x0002 d_int16  >>= fun s_x_height ->
    opt 0x0002 d_int16  >>= fun s_cap_height ->
    opt 0x0002 d_uint16 >>= fun us_default_char ->
    opt 0x0002 d_uint16 >>= fun us_break_char ->
    opt 0x0002 d_uint16 >>= fun us_max_context ->
    opt 0x0005 d_uint16 >>= fun us_lower_optical_point_size ->
    opt 0x0005 d_uint16 >>= fun us_upper_optical_point_size ->
    return {
      value = Value.Os2.{
        us_weight_class;
        us_width_class;
        fs_type;
        y_subscript_x_size;
        y_subscript_y_size;
        y_subscript_x_offset;
        y_subscript_y_offset;
        y_superscript_x_size;
        y_superscript_y_size;
        y_superscript_x_offset;
        y_superscript_y_offset;
        y_strikeout_size;
        y_strikeout_position;
        s_family_class;
        panose;
        ach_vend_id;
        fs_selection;
        s_typo_ascender;
        s_type_descender;
        s_typo_linegap;
        us_win_ascent;
        us_win_descent;
        ul_code_page_range_1;
        ul_code_page_range_2;
        s_x_height;
        s_cap_height;
        us_default_char;
        us_break_char;
        us_lower_optical_point_size;
        us_upper_optical_point_size;
      };
      derived = {
        x_avg_char_width;
        ul_unicode_range1;
        ul_unicode_range2;
        ul_unicode_range3;
        ul_unicode_range4;
        us_first_char_index;
        us_last_char_index;
        us_max_context;
      };
    }
  in
  DecodeOperation.run common.core offset dec
