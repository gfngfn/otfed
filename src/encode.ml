
open Basic
open Value
open EncodeOperation.Open


include EncodeBasic


module Subset = EncodeSubset


module Head = struct

  let e_mac_style (mac_style : Value.Head.mac_style) : unit encoder =
    let open EncodeOperation in
    let open Value.Head in
    e_16bits [
      mac_style.bold;
      mac_style.italic;
      mac_style.underline;
      mac_style.outline;
      mac_style.shadow;
      mac_style.condensed;
      mac_style.extended;
    ]


  let make (ihead : Intermediate.Head.t) : table ok =
    let d = ihead.Intermediate.Head.derived in
    let v = ihead.Intermediate.Head.value in
    let loc_format_num =
      let open Intermediate in
      match d.index_to_loc_format with
      | ShortLocFormat -> 0
      | LongLocFormat  -> 1
    in
    let table_version = !% 0x00010000 in
    let magic_number = !% 0x5F0F3CF5 in
    let font_direction_hint = 0 in
    let glyph_data_format = 0 in
    let enc =
      let open EncodeOperation in
      e_uint32 table_version      >>= fun () ->
      e_uint32 v.font_revision    >>= fun () ->
      e_uint32 (!% 0)             >>= fun () -> (* `checkSumAdjustment`: will be updated afterwards *)
      e_uint32 magic_number       >>= fun () ->
      e_uint16 v.flags            >>= fun () ->
      e_uint16 v.units_per_em     >>= fun () ->
      e_timestamp v.created       >>= fun () ->
      e_timestamp v.modified      >>= fun () ->
      e_int16 d.x_min             >>= fun () ->
      e_int16 d.y_min             >>= fun () ->
      e_int16 d.x_max             >>= fun () ->
      e_int16 d.y_max             >>= fun () ->
      e_mac_style v.mac_style     >>= fun () ->
      e_uint16 v.lowest_rec_ppem  >>= fun () ->
      e_int16 font_direction_hint >>= fun () ->
      e_int16 loc_format_num      >>= fun () ->
      e_int16 glyph_data_format   >>= fun () ->
      return ()
    in
    let open ResultMonad in
    enc |> EncodeOperation.run >>= fun (contents, ()) ->
    return {
      tag = Value.Tag.table_head;
      contents;
    }

end


module Hhea = struct

  let make ~number_of_h_metrics (ihhea : Intermediate.Hhea.t) : table ok =
    let d = ihhea.Intermediate.Hhea.derived in
    let v = ihhea.Intermediate.Hhea.value in
    let table_version = !% 0x00010000 in
    let metric_data_format = 0 in
    let enc =
      let open EncodeOperation in
      e_uint32 table_version            >>= fun () ->
      e_int16  v.ascender               >>= fun () ->
      e_int16  v.descender              >>= fun () ->
      e_int16  v.line_gap               >>= fun () ->
      e_uint16 d.advance_width_max      >>= fun () ->
      e_int16  d.min_left_side_bearing  >>= fun () ->
      e_int16  d.min_right_side_bearing >>= fun () ->
      e_int16  d.xmax_extent            >>= fun () ->
      e_int16  v.caret_slope_rise       >>= fun () ->
      e_int16  v.caret_slope_run        >>= fun () ->
      e_int16  v.caret_offset           >>= fun () ->
      e_int16  0                        >>= fun () ->
      e_int16  0                        >>= fun () ->
      e_int16  0                        >>= fun () ->
      e_int16  0                        >>= fun () ->
      e_int16  metric_data_format       >>= fun () ->
      e_uint16 number_of_h_metrics      >>= fun () ->
      return ()
    in
    let open ResultMonad in
    enc |> EncodeOperation.run >>= fun (contents, ()) ->
    return {
      tag = Value.Tag.table_hhea;
      contents;
    }

end


module Os2 = struct

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
      e_uint32 d.ul_unicode_range1      >>= fun () ->
      e_uint32 d.ul_unicode_range2      >>= fun () ->
      e_uint32 d.ul_unicode_range3      >>= fun () ->
      e_uint32 d.ul_unicode_range4      >>= fun () ->
      e_bytes  v.ach_vend_id            >>= fun () -> (* asserted to be 4 bytes long *)
      e_uint16 v.fs_selection           >>= fun () ->
      e_uint16 d.us_first_char_index    >>= fun () ->
      e_uint16 d.us_last_char_index     >>= fun () ->
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

end


module Hmtx = struct

  let make (entries : (int * int) list) : table ok =
    let enc =
      let open EncodeOperation in
      let e_entry (advanceWidth, lsb) =
        e_uint16 advanceWidth >>= fun () ->
        e_int16  lsb
      in
      e_list e_entry entries
    in
    let open ResultMonad in
    enc |> EncodeOperation.run >>= fun (contents, ()) ->
    return {
      tag = Value.Tag.table_hmtx;
      contents;
    }


  let make_by_iter ~num_glyphs:(num_glyphs : int) (f : glyph_id -> int * int) : table ok =
    let enc =
      let open EncodeOperation in
      let rec aux gid =
        if gid >= num_glyphs then
          return ()
        else
          let (advanceWidth, lsb) = f gid in
          e_uint16 advanceWidth >>= fun () ->
          e_int16  lsb          >>= fun () ->
          aux (gid + 1)
      in
      aux 0
    in
    let open ResultMonad in
    enc |> EncodeOperation.run >>= fun (contents, ()) ->
    return {
      tag = Value.Tag.table_hmtx;
      contents;
    }

end


module Ttf = struct

  module Maxp = EncodeTtfMaxp


  type glyph_info = {
    bounding_box : Value.bounding_box;
    description  : Value.ttf_glyph_description;
  }


  let make_end_points (contours : ttf_contour list) : int list =
    let (acc, _) =
      contours |> List.fold_left (fun (acc, i) contour ->
        let num_points_in_contour = List.length contour in
        let i_next = i + num_points_in_contour in
        (Alist.extend acc (i_next - 1), i_next)
      ) (Alist.empty, 0)
    in
    Alist.to_list acc


  type flag = Intermediate.Ttf.flag

  type relative =
    | Short of int (* non-negative *)
    | Long  of int

  type coordinate_accumulator = {
    flags       : flag Alist.t;
    relative_xs : relative Alist.t;
    relative_ys : relative Alist.t;
  }


  let decompose_contours (contours : ttf_contour list) : flag list * relative list * relative list =
    let coordinates = List.concat contours in
    let acc =
      {
        flags       = Alist.empty;
        relative_xs = Alist.empty;
        relative_ys = Alist.empty;
      }
    in
    let (acc, _, _) =
      coordinates |> List.fold_left (fun (acc, x_coord_prev, y_coord_prev) (on_curve, x_coord, y_coord) ->
        let (x_short_vector, this_x_is_same, relative_xs) =
          let x_rel = x_coord - x_coord_prev in
          if x_rel = 0 then
            (false, true, acc.relative_xs)
          else if -256 <= x_rel && x_rel < 256 then
            let (is_positive, x_rel_abs) =
              if x_rel > 0 then (true, x_rel) else (false, -x_rel)
            in
            (true, is_positive, Alist.extend acc.relative_xs (Short(x_rel_abs)))
          else
            (false, false, Alist.extend acc.relative_xs (Long(x_rel)))
        in
        let (y_short_vector, this_y_is_same, relative_ys) =
          let y_rel = y_coord - y_coord_prev in
          if y_rel = 0 then
            (false, true, acc.relative_ys)
          else if -256 <= y_rel && y_rel < 256 then
            let (is_positive, y_rel_abs) =
              if y_rel > 0 then (true, y_rel) else (false, -y_rel)
            in
            (true, is_positive, Alist.extend acc.relative_ys (Short(y_rel_abs)))
          else
            (false, false, Alist.extend acc.relative_ys (Long(y_rel)))
        in
        let flag =
          Intermediate.Ttf.{
            on_curve;
            this_x_is_same;
            this_y_is_same;
            x_short_vector;
            y_short_vector;
          }
        in
        let acc =
          {
            flags = Alist.extend acc.flags flag;
            relative_xs;
            relative_ys;
          }
        in
        (acc, x_coord, y_coord)
      ) (acc, 0, 0)
    in
    (Alist.to_list acc.flags, Alist.to_list acc.relative_xs, Alist.to_list acc.relative_ys)


  let e_flag (flag : flag) =
    let open EncodeOperation in
    let open Intermediate.Ttf in
    e_8bits [
      flag.on_curve;
      flag.x_short_vector;
      flag.y_short_vector;
      flag.this_x_is_same;
      flag.this_y_is_same;
    ]


  let e_relative (relative : relative) =
    let open EncodeOperation in
    match relative with
    | Short(rel_abs) -> e_uint8 rel_abs
    | Long(rel)      -> e_int16 rel


  let e_simple_glyph (contours : ttf_simple_glyph_description) : unit encoder =
    let endPtsOfContours = make_end_points contours in
    let instructions = "" in (* TODO: `instructions` should be extracted from `ttf_simple_glyph_description`. *)
    let instructionLength = String.length instructions in
    let (flags, xCoordinates, yCoordinates) = decompose_contours contours in
    let open EncodeOperation in
    e_list e_uint16 endPtsOfContours >>= fun () ->
    e_uint16 instructionLength       >>= fun () ->
    e_bytes instructions             >>= fun () ->
    e_list e_flag flags              >>= fun () ->
    e_list e_relative xCoordinates   >>= fun () ->
    e_list e_relative yCoordinates   >>= fun () ->
    return ()


  let e_composite_glyph (_elems : ttf_composite_glyph_description) : unit encoder =
    failwith "TODO: e_composite_glyph"


  let e_glyph (g : glyph_info) =
    let open EncodeOperation in
    let (number_of_contours, enc) =
      match g.description with
      | TtfSimpleGlyph(contours) -> (List.length contours, e_simple_glyph contours)
      | TtfCompositeGlyph(elems) -> (-1, e_composite_glyph elems)
    in
    e_int16 number_of_contours   >>= fun () ->
    e_int16 g.bounding_box.x_min >>= fun () ->
    e_int16 g.bounding_box.y_min >>= fun () ->
    e_int16 g.bounding_box.x_max >>= fun () ->
    e_int16 g.bounding_box.y_max >>= fun () ->
    enc

end


module Cff = struct

  module Maxp = EncodeCffMaxp

end
