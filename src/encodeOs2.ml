
open Basic
open EncodeBasic


type extension5_contents = {
  us_lower_optical_point_size : int;
  us_upper_optical_point_size : int;
}

type extension5 =
  extension5_contents * unit

type extension2_contents = {
  s_x_height      : int;
  s_cap_height    : int;
  us_default_char : Uchar.t;
  us_break_char   : Uchar.t;
  us_max_context  : int;
}

type extension2 =
  extension2_contents * extension5 option

type extension1_contents = {
  ul_code_page_range1 : wint;
  ul_code_page_range2 : wint;
}

type extension1 =
  extension1_contents * extension2 option

type extension =
  extension1 option


let form_extension_structure (d : Intermediate.Os2.derived) (v : Value.Os2.t) : extension =
  match
    ( v.ul_code_page_range1,
      v.ul_code_page_range2 )
  with
  | ( Some(ul_code_page_range1),
      Some(ul_code_page_range2) ) ->
      let extension1_contents =
        {
          ul_code_page_range1;
          ul_code_page_range2;
        }
      in
      let extension2_option =
        match
          ( v.s_x_height,
            v.s_cap_height,
            v.us_default_char,
            v.us_break_char,
            d.us_max_context )
        with
        | ( Some(s_x_height),
            Some(s_cap_height),
            Some(us_default_char),
            Some(us_break_char),
            Some(us_max_context) ) ->
            let extension2_contents =
              {
                s_x_height;
                s_cap_height;
                us_default_char;
                us_break_char;
                us_max_context;
              }
            in
            let extension5_option =
              match
                ( v.us_lower_optical_point_size,
                  v.us_upper_optical_point_size )
              with
              | ( Some(us_lower_optical_point_size),
                  Some(us_upper_optical_point_size) ) ->
                  let extension5_contents =
                    {
                      us_lower_optical_point_size;
                      us_upper_optical_point_size;
                    }
                  in
                  Some((extension5_contents, ()))

              | _ ->
                  None
            in
            Some((extension2_contents, extension5_option))

        | _ ->
            None
      in
      Some((extension1_contents, extension2_option))

  | _ ->
      None


let e_opt enc x =
  let open EncodeOperation in
  match x with
  | None    -> return ()
  | Some(v) -> enc v


let e_extension5 ((ext5, ()) : extension5) =
  let open EncodeOperation in
  e_uint16 ext5.us_lower_optical_point_size >>= fun () ->
  e_uint16 ext5.us_upper_optical_point_size >>= fun () ->
  return ()


let e_extension2 ((ext2, ext5_option) : extension2) =
  let open EncodeOperation in
  e_int16  ext2.s_x_height       >>= fun () ->
  e_int16  ext2.s_cap_height     >>= fun () ->
  e_bmp_code_point ext2.us_default_char >>= fun () ->
  e_bmp_code_point ext2.us_break_char   >>= fun () ->
  e_uint16 ext2.us_max_context   >>= fun () ->
  e_opt e_extension5 ext5_option >>= fun () ->
  return ()


let e_extension1 ((ext1, ext2_option) : extension1) =
  let open EncodeOperation in
  e_uint32 ext1.ul_code_page_range1 >>= fun () ->
  e_uint32 ext1.ul_code_page_range2 >>= fun () ->
  e_opt e_extension2 ext2_option    >>= fun () ->
  return ()


let e_weight_class (weight_class : int) =
  let open EncodeOperation in
  if (1 <= weight_class && weight_class <= 1000) then
    e_uint16 weight_class
  else
    err @@ InvalidWeightClass(weight_class)


let e_width_class (width_class : Value.Os2.width_class) =
  let open EncodeOperation in
  let u =
    match width_class with
    | Value.Os2.WidthUltraCondensed -> 1
    | Value.Os2.WidthExtraCondensed -> 2
    | Value.Os2.WidthCondensed      -> 3
    | Value.Os2.WidthSemiCondensed  -> 4
    | Value.Os2.WidthMedium         -> 5
    | Value.Os2.WidthSemiExpanded   -> 6
    | Value.Os2.WidthExpanded       -> 7
    | Value.Os2.WidthExtraExpanded  -> 8
    | Value.Os2.WidthUltraExpanded  -> 9
  in
  e_uint16 u


let e_fs_type (fs_type : Value.Os2.fs_type) =
  let open EncodeOperation in
  let u = 0 in
  let u = if fs_type.restricted_license_embedding then u + 0x2 else u in
  let u = if fs_type.preview_and_print_embedding  then u + 0x4 else u in
  let u = if fs_type.editable_embedding           then u + 0x8 else u in
  let u = if fs_type.no_subsetting                then u + 0x100 else u in
  let u = if fs_type.bitmap_embedding_only        then u + 0x200 else u in
  e_uint16 u


let e_fs_selection (fs_selection : Value.Os2.fs_selection) =
  let open EncodeOperation in
  let u = 0 in
  let u = if fs_selection.italic           then u + 1 else u in
  let u = if fs_selection.underscore       then u + 2 else u in
  let u = if fs_selection.negative         then u + 4 else u in
  let u = if fs_selection.outlined         then u + 8 else u in
  let u = if fs_selection.strikeout        then u + 16 else u in
  let u = if fs_selection.bold             then u + 32 else u in
  let u = if fs_selection.regular          then u + 64 else u in
  let u = if fs_selection.use_typo_metrics then u + 128 else u in
  let u = if fs_selection.wws              then u + 256 else u in
  let u = if fs_selection.oblique          then u + 512 else u in
  e_uint16 u


let fs_selection_requires_table_version4_or_later (fs_selection : Value.Os2.fs_selection) : bool =
  let Value.Os2.{ use_typo_metrics; wws; oblique; _ } = fs_selection in
  use_typo_metrics || wws || oblique


let e_os2 (ios2 : Intermediate.Os2.t) =
  let open EncodeOperation in
  let d = ios2.Intermediate.Os2.derived in
  let v = ios2.Intermediate.Os2.value in
  begin
    if String.length v.panose <> 10 then
      err @@ Error.NotA10BytePanose(v.panose)
    else
      return ()
  end >>= fun () ->
  begin
    if String.length v.ach_vend_id <> 4 then
      err @@ Error.NotA4ByteAchVendId(v.ach_vend_id)
    else
      return ()
  end >>= fun () ->
  let extension = form_extension_structure d v in
  let fs_selection = v.fs_selection in
  let must_be_version4_or_later = fs_selection_requires_table_version4_or_later fs_selection in
  begin
    match extension with
    | None ->
        if must_be_version4_or_later then
          err @@ Version4FsSelection(fs_selection)
        else
          return 0x0000

    | Some((_, None)) ->
        if must_be_version4_or_later then
          err @@ Version4FsSelection(fs_selection)
        else
          return 0x0001

    | Some((_, Some((_, None)))) ->
        if must_be_version4_or_later then
          return 0x0004
        else
          return 0x0002

    | Some((_, Some((_, Some((_, ())))))) ->
        return 0x0005

  end >>= fun table_version ->
  e_uint16 table_version            >>= fun () ->
  e_int16  d.x_avg_char_width       >>= fun () ->
  e_weight_class v.us_weight_class  >>= fun () ->
  e_width_class v.us_width_class    >>= fun () ->
  e_fs_type v.fs_type               >>= fun () ->
  e_int16  v.y_subscript_x_size     >>= fun () ->
  e_int16  v.y_subscript_y_size     >>= fun () ->
  e_int16  v.y_subscript_x_offset   >>= fun () ->
  e_int16  v.y_subscript_y_offset   >>= fun () ->
  e_int16  v.y_superscript_x_size   >>= fun () ->
  e_int16  v.y_superscript_y_size   >>= fun () ->
  e_int16  v.y_superscript_x_offset >>= fun () ->
  e_int16  v.y_superscript_y_offset >>= fun () ->
  e_int16  v.y_strikeout_size       >>= fun () ->
  e_int16  v.y_strikeout_position   >>= fun () ->
  e_int16  v.s_family_class         >>= fun () ->
  e_bytes  v.panose                 >>= fun () -> (* asserted to be 10 bytes long *)
  e_uint32 v.ul_unicode_range1      >>= fun () ->
  e_uint32 v.ul_unicode_range2      >>= fun () ->
  e_uint32 v.ul_unicode_range3      >>= fun () ->
  e_uint32 v.ul_unicode_range4      >>= fun () ->
  e_bytes  v.ach_vend_id            >>= fun () -> (* asserted to be 4 bytes long *)
  e_fs_selection fs_selection            >>= fun () ->
  e_bmp_code_point d.us_first_char_index >>= fun () ->
  e_bmp_code_point d.us_last_char_index  >>= fun () ->
  e_int16  v.s_typo_ascender        >>= fun () ->
  e_int16  v.s_typo_descender       >>= fun () ->
  e_int16  v.s_typo_linegap         >>= fun () ->
  e_uint16 v.us_win_ascent          >>= fun () ->
  e_uint16 v.us_win_descent         >>= fun () ->
  e_opt e_extension1 extension      >>= fun () ->
  return ()


let make (ios2 : Intermediate.Os2.t) : table ok =
  let open ResultMonad in
  e_os2 ios2 |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_os2;
    contents;
  }
