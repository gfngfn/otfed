
open Basic
open DecodeBasic
open DecodeOperation.Open


let d_weight_class : int decoder =
  let open DecodeOperation in
  d_uint16 >>= fun n ->
  if (1 <= n && n <= 1000) then
    return n
  else
    err @@ InvalidWeightClass(n)


let d_width_class : Value.Os2.width_class decoder =
  let open DecodeOperation in
  d_uint16 >>= function
  | 1 -> return Value.Os2.WidthUltraCondensed
  | 2 -> return Value.Os2.WidthExtraCondensed
  | 3 -> return Value.Os2.WidthCondensed
  | 4 -> return Value.Os2.WidthSemiCondensed
  | 5 -> return Value.Os2.WidthMedium
  | 6 -> return Value.Os2.WidthSemiExpanded
  | 7 -> return Value.Os2.WidthExpanded
  | 8 -> return Value.Os2.WidthExtraExpanded
  | 9 -> return Value.Os2.WidthUltraExpanded
  | n -> err @@ UnknownWidthClass(n)


let d_fs_type : Value.Os2.fs_type decoder =
  let open DecodeOperation in
  d_uint16 >>= fun u ->
    (* Checks that the 1st, 4th-7th, and 10th-15th bits are all zero: *)
  if u land (1 + 16 + 32 + 64 + 128 + 1024 + 2048 + 4096 + 8192 + 16384 + 32768) > 0 then
    err @@ InvalidFsType(u)
  else
    return Value.Os2.{
      restricted_license_embedding = (u land 2 > 0);
      preview_and_print_embedding  = (u land 4 > 0);
      editable_embedding           = (u land 8 > 0);
      no_subsetting                = (u land 256 > 0);
      bitmap_embedding_only        = (u land 512 > 0);
    }


let d_fs_selection : Value.Os2.fs_selection decoder =
  let open DecodeOperation in
  d_uint16 >>= fun u ->
  if u land (1024 + 2048 + 4096 + 8192 + 16384 + 32768) > 0 then
    err @@ InvalidFsSelection(u)
  else
    return Value.Os2.{
      italic           = (u land 1 > 0);
      underscore       = (u land 2 > 0);
      negative         = (u land 4 > 0);
      outlined         = (u land 8 > 0);
      strikeout        = (u land 16 > 0);
      bold             = (u land 32 > 0);
      regular          = (u land 64 > 0);
      use_typo_metrics = (u land 128 > 0);
      wws              = (u land 256 > 0);
      oblique          = (u land 512 > 0);
    }


let d_os2 =
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
  d_weight_class >>= fun us_weight_class ->
  d_width_class  >>= fun us_width_class ->
  d_fs_type      >>= fun fs_type ->
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
  d_fs_selection   >>= fun fs_selection ->
  d_bmp_code_point >>= fun us_first_char_index ->
  d_bmp_code_point >>= fun us_last_char_index ->
  d_int16    >>= fun s_typo_ascender ->
  d_int16    >>= fun s_typo_descender ->
  d_int16    >>= fun s_typo_linegap ->
  d_uint16   >>= fun us_win_ascent ->
  d_uint16   >>= fun us_win_descent ->
  opt 0x0001 d_uint32 >>= fun ul_code_page_range1 ->
  opt 0x0001 d_uint32 >>= fun ul_code_page_range2 ->
  opt 0x0002 d_int16  >>= fun s_x_height ->
  opt 0x0002 d_int16  >>= fun s_cap_height ->
  opt 0x0002 d_bmp_code_point >>= fun us_default_char ->
  opt 0x0002 d_bmp_code_point >>= fun us_break_char ->
  opt 0x0002 d_uint16 >>= fun us_max_context ->
  opt 0x0005 d_uint16 >>= fun us_lower_optical_point_size ->
  opt 0x0005 d_uint16 >>= fun us_upper_optical_point_size ->
  return Intermediate.Os2.{
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
      ul_unicode_range1;
      ul_unicode_range2;
      ul_unicode_range3;
      ul_unicode_range4;
      ach_vend_id;
      fs_selection;
      s_typo_ascender;
      s_typo_descender;
      s_typo_linegap;
      us_win_ascent;
      us_win_descent;
      ul_code_page_range1;
      ul_code_page_range2;
      s_x_height;
      s_cap_height;
      us_default_char;
      us_break_char;
      us_lower_optical_point_size;
      us_upper_optical_point_size;
    };
    derived = {
      x_avg_char_width;
      us_first_char_index;
      us_last_char_index;
      us_max_context;
    };
  }


let get (src : source) : Intermediate.Os2.t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_os2 >>= fun (offset, _length) ->
  DecodeOperation.run common.core offset d_os2
