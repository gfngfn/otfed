
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
  us_default_char : int;
  us_break_char   : int;
  us_max_context  : int;
}

type extension2 =
  extension2_contents * extension5 option

type extension1_contents = {
  ul_code_page_range_1 : wint;
  ul_code_page_range_2 : wint;
}

type extension1 =
  extension1_contents * extension2 option

type extension =
  extension1 option

let form_extension_structure (d : Intermediate.Os2.derived) (v : Value.Os2.t) : extension =
  match
    ( v.ul_code_page_range_1,
      v.ul_code_page_range_2 )
  with
  | ( Some(ul_code_page_range_1),
      Some(ul_code_page_range_2) ) ->
      let extension1_contents =
        {
          ul_code_page_range_1;
          ul_code_page_range_2;
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
  e_uint16 ext2.us_default_char  >>= fun () ->
  e_uint16 ext2.us_break_char    >>= fun () ->
  e_uint16 ext2.us_max_context   >>= fun () ->
  e_opt e_extension5 ext5_option >>= fun () ->
  return ()


let e_extension1 ((ext1, ext2_option) : extension1) =
  let open EncodeOperation in
  e_uint32 ext1.ul_code_page_range_1 >>= fun () ->
  e_uint32 ext1.ul_code_page_range_2 >>= fun () ->
  e_opt e_extension2 ext2_option     >>= fun () ->
  return ()


let make (ios2 : Intermediate.Os2.t) : table ok =
  let d = ios2.Intermediate.Os2.derived in
  let v = ios2.Intermediate.Os2.value in
  let extension = form_extension_structure d v in
  let table_version =
    match extension with
    | None                                -> 0x0000
    | Some((_, None))                     -> 0x0001
    | Some((_, Some((_, None))))          -> 0x0002
    | Some((_, Some((_, Some((_, ())))))) -> 0x0005
  in
  let open ResultMonad in
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
  let enc =
    let open EncodeOperation in
    e_uint16 table_version            >>= fun () ->
    e_int16  d.x_avg_char_width       >>= fun () ->
    e_uint16 v.us_weight_class        >>= fun () ->
    e_uint16 v.us_width_class         >>= fun () ->
    e_uint16 v.fs_type                >>= fun () ->
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
    e_uint16 v.fs_selection           >>= fun () ->
    e_bmp_code_point d.us_first_char_index >>= fun () ->
    e_bmp_code_point d.us_last_char_index  >>= fun () ->
    e_int16  v.s_typo_ascender        >>= fun () ->
    e_int16  v.s_type_descender       >>= fun () ->
    e_int16  v.s_typo_linegap         >>= fun () ->
    e_uint16 v.us_win_ascent          >>= fun () ->
    e_uint16 v.us_win_descent         >>= fun () ->
    e_opt e_extension1 extension      >>= fun () ->
    return ()
  in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_os2;
    contents;
  }
