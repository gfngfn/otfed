
open Basic
open Value
open DecodeOperation


include DecodeBasic


let fetch_loc_format core table_directory =
  let open ResultMonad in
  seek_required_table table_directory Value.Tag.table_head >>= fun (offset, _length) ->
  run core (offset + 50) d_loc_format


let d_init_ttf core =
  d_structure >>= fun table_directory ->
  transform_result (fetch_loc_format core table_directory) >>= fun loc_format ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
      loc_format      = loc_format;
    }
  in
  let ttf = {ttf_common = common} in
  return @@ (common, Ttf(ttf))


let d_init_cff (core : common_source_core) : source decoder =
  d_structure >>= fun table_directory ->
  transform_result (fetch_loc_format core table_directory) >>= fun loc_format ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
      loc_format      = loc_format;
    }
  in
  let cff = {cff_common = common} in
  return @@ (common, Cff(cff))


let source_of_string (s : string) : single_or_collection ok =
  let core =
    {
      data = s;
      max  = String.length s - 1;
    }
  in
  let dec =
    d_format_version >>= fun format ->
    match format with
    | InitTtf ->
        d_init_ttf core >>= fun src ->
        return @@ Single(src)

    | InitCff ->
        d_init_cff core >>= fun src ->
        return @@ Single(src)

    | InitCollection ->
        d_ttc_header_offset_list >>= fun offsets ->
        offsets |> mapM (fun offset ->
          seek offset >>= fun () ->
          d_format_version >>= fun format ->
          match format with
          | InitTtf ->
              d_init_ttf core

          | InitCff ->
              d_init_cff core

          | InitCollection ->
              err LayeredTtc
        ) >>= fun srcs ->
        return @@ Collection(srcs)
  in
  run core 0 dec


let tables (common : common_source) : Value.Tag.t set =
  let acc =
    TableDirectory.fold (fun tag _ acc ->
      Alist.extend acc tag
    ) common.table_directory Alist.empty
  in
  acc |> Alist.to_list


module Intermediate = DecodeIntermediate


let cmap (common : common_source) : Intermediate.Cmap.t ok =
  let open ResultMonad in
  seek_required_table common.table_directory Value.Tag.table_cmap >>= fun (offset, length) ->
  return @@ Intermediate.Cmap.make common.core offset length


let head (common : common_source) : Value.Head.t ok =
  let open ResultMonad in
  seek_required_table common.table_directory Value.Tag.table_head >>= fun (offset, _length) ->
  let dec =
    let open DecodeOperation in
    d_uint32 >>= fun version ->
    if version <> !%% 0x00010000L then
      err @@ UnknownTableVersion(version)
    else
      d_uint32     >>= fun font_revision ->
      d_skip 8     >>= fun () -> (* Skips `checkSumAdjustement` and `magicNumber`. *)
      d_uint16     >>= fun flags ->
      d_uint16     >>= fun units_per_em ->
      d_timestamp  >>= fun created ->
      d_timestamp  >>= fun modified ->
      d_int16      >>= fun xmin ->
      d_int16      >>= fun ymin ->
      d_int16      >>= fun xmax ->
      d_int16      >>= fun ymax ->
      d_uint16     >>= fun mac_style ->
      d_uint16     >>= fun lowest_rec_ppem ->
      (* Skips `fontDirectionHint` and `indexToLocFormat`. *)
      return Value.Head.{
        font_revision;
        flags;
        units_per_em;
        created;
        modified;
        xmin;
        ymin;
        xmax;
        ymax;
        mac_style;
        lowest_rec_ppem;
      }
  in
  DecodeOperation.run common.core offset dec


let hhea (common : common_source) : Value.Hhea.t ok =
  let open ResultMonad in
  seek_required_table common.table_directory Value.Tag.table_hhea >>= fun (offset, _length) ->
  let dec =
    let open DecodeOperation in
    d_uint32 >>= fun version ->
    if version <> !%% 0x00010000L then
      err @@ UnknownTableVersion(version)
    else
      d_int16  >>= fun ascender ->
      d_int16  >>= fun descender ->
      d_int16  >>= fun line_gap ->
      d_uint16 >>= fun advance_width_max ->
      d_int16  >>= fun min_left_side_bearing ->
      d_int16  >>= fun min_right_side_bearing ->
      d_int16  >>= fun xmax_extent ->
      d_int16  >>= fun caret_slope_rise ->
      d_int16  >>= fun caret_slope_run ->
      d_int16  >>= fun caret_offset ->
      return Value.Hhea.{
        ascender;
        descender;
        line_gap;
        advance_width_max;
        min_left_side_bearing;
        min_right_side_bearing;
        xmax_extent;
        caret_slope_rise;
        caret_slope_run;
        caret_offset;
      }
  in
  DecodeOperation.run common.core offset dec


let os2 (common : common_source) : Value.Os2.t ok =
  let open ResultMonad in
  seek_required_table common.table_directory Value.Tag.table_os2 >>= fun (offset, _length) ->
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
    return Value.Os2.{
      x_avg_char_width;
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
      us_first_char_index;
      us_last_char_index;
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
      us_max_context;
      us_lower_optical_point_size;
      us_upper_optical_point_size;
    }
  in
  DecodeOperation.run common.core offset dec


let maxp (common : common_source) : Value.Maxp.t ok =
  let open ResultMonad in
  seek_required_table common.table_directory Value.Tag.table_maxp >>= fun (offset, _length) ->
  let dec =
    let open DecodeOperation in
    d_uint32 >>= fun version ->
    if version = !%% 0x00005000L then
    (* If the font is based on CFF *)
      d_uint16 >>= fun num_glyphs ->
      return Value.Maxp.{
        num_glyphs               = num_glyphs;
        max_points               = 0;
        max_contours             = 0;
        max_composite_points     = 0;
        max_composite_contours   = 0;
        max_zones                = 0;
        max_twilight_points      = 0;
        max_storage              = 0;
        max_function_defs        = 0;
        max_instruction_defs     = 0;
        max_stack_elements       = 0;
        max_size_of_instructions = 0;
        max_component_elements   = 0;
        max_component_depth      = 0;
      }
    else if version = !%% 0x00010000L then
    (* If the font is based on TTF *)
      d_uint16 >>= fun num_glyphs ->
      d_uint16 >>= fun max_points ->
      d_uint16 >>= fun max_contours ->
      d_uint16 >>= fun max_composite_points ->
      d_uint16 >>= fun max_composite_contours ->
      d_uint16 >>= fun max_zones ->
      d_uint16 >>= fun max_twilight_points ->
      d_uint16 >>= fun max_storage ->
      d_uint16 >>= fun max_function_defs ->
      d_uint16 >>= fun max_instruction_defs ->
      d_uint16 >>= fun max_stack_elements ->
      d_uint16 >>= fun max_size_of_instructions ->
      d_uint16 >>= fun max_component_elements ->
      d_uint16 >>= fun max_component_depth ->
      return Value.Maxp.{
        num_glyphs;
        max_points;
        max_contours;
        max_composite_points;
        max_composite_contours;
        max_zones;
        max_twilight_points;
        max_storage;
        max_function_defs;
        max_instruction_defs;
        max_stack_elements;
        max_size_of_instructions;
        max_component_elements;
        max_component_depth;
      }
    else
      err @@ Error.UnknownTableVersion(version)
  in
  DecodeOperation.run common.core offset dec


let d_loca_short (gid : glyph_id) : ((int * int) option) decoder =
  let open DecodeOperation in
  (* The position is set to the begging of `loca` table. *)
  d_skip (gid * 2) >>= fun () ->
  d_uint16 >>= fun half1 ->
  d_uint16 >>= fun half2 ->
  if half1 = half2 then
    return None
  else
    let glyf_offset1 = half1 * 2 in
    let glyf_offset2 = half2 * 2 in
    return @@ Some(glyf_offset1, glyf_offset2 - glyf_offset1)


let d_loca_long (gid : glyph_id) : ((int * int) option) decoder =
  let open DecodeOperation in
  (* The position is set to the begging of `loca` table. *)
  d_skip (gid * 4) >>= fun () ->
  d_uint32_int >>= fun glyf_offset1 ->
  d_uint32_int >>= fun glyf_offset2 ->
  if glyf_offset1 = glyf_offset2 then
    return None
  else
    return @@ Some(glyf_offset1, glyf_offset2 - glyf_offset1)


let d_loca (loc_format : loc_format) (gid : glyph_id) : (ttf_glyph_location option) decoder =
  let open DecodeOperation in
  (* The position is set to the begging of `loca` table. *)
  let dec =
    match loc_format with
    | ShortLocFormat -> d_loca_short gid
    | LongLocFormat  -> d_loca_long gid
  in
  dec >>= function
  | None                         -> return None
  | Some((glyf_offset, _length)) -> return @@ Some(TtfGlyphLocation(glyf_offset))


let loca (ttf : ttf_source) (gid : glyph_id) : (ttf_glyph_location option) ok =
  let open ResultMonad in
  let common = ttf.ttf_common in
  seek_required_table common.table_directory Value.Tag.table_loca >>= fun (offset, _length) ->
  DecodeOperation.run common.core offset (d_loca common.loc_format gid)


let d_end_points (numberOfContours : int) : (int Alist.t) decoder =
  let open DecodeOperation in
  let rec loop i acc =
    if i <= 0 then
      return acc
    else
      d_uint16 >>= fun e ->
      loop (i - 1) (Alist.extend acc e)
  in
  loop numberOfContours Alist.empty


type flag = {
  on_curve       : bool;
  x_short_vector : bool;
  y_short_vector : bool;
  this_x_is_same : bool;
  this_y_is_same : bool;
}


let d_flags (num_points : int) : (flag Alist.t) decoder =
  let rec extend_repeatedly acc n x =
    if n <= 0 then
      acc
    else
      extend_repeatedly (Alist.extend acc x) (n - 1) x
  in
  let rec aux i acc =
    let open DecodeOperation in
    if i <= 0 then
      return acc
    else
      d_uint8 >>= fun byte ->
      let flag =
        {
          on_curve       = (byte land 1 > 0);
          x_short_vector = (byte land 2 > 0);
          y_short_vector = (byte land 4 > 0);
          this_x_is_same = (byte land 16 > 0);
          this_y_is_same = (byte land 32 > 0);
        }
      in
      let does_repeat = (byte land 8 > 0) in
      if does_repeat then
        d_uint8 >>= fun n ->
        aux (i - 1 - n) (extend_repeatedly acc (n + 1) flag)
      else
        aux (i - 1) (Alist.extend acc flag)
  in
  aux num_points Alist.empty


let d_coordinates is_short is_same (flags : flag list) : (int list) decoder =
  let open DecodeOperation in
  let rec aux x acc = function
    | [] ->
        return @@ Alist.to_list acc

    | flag :: flags ->
        begin
          if is_short flag then
            d_uint8 >>= fun dx ->
            return @@ x + (if is_same flag then dx else - dx)
          else
            if is_same flag then
              return x
            else
              d_int16 >>= fun dx ->
              return @@ x + dx
        end >>= fun x ->
        aux x (Alist.extend acc x) flags

  in
  aux 0 Alist.empty flags


let d_x_coordinates flags =
  d_coordinates (fun flag -> flag.x_short_vector) (fun flag -> flag.this_x_is_same) flags

let d_y_coordinates flags =
  d_coordinates (fun flag -> flag.y_short_vector) (fun flag -> flag.this_y_is_same) flags


let combine (endPtsOfContours : int list) (num_points : int) (flags : flag list) (xCoordinates : int list) (yCoordinates : int list) =
  let rec aux pointacc contouracc endPtsOfContours = function
    | (i, [], [], []) ->
        assert (i = num_points);
        assert (Alist.is_empty pointacc);
        Alist.to_list contouracc

    | (
        i,
        flag        :: flags,
        xCoordinate :: xCoordinates,
        yCoordinate :: yCoordinates
      ) ->
        let point = (flag.on_curve, xCoordinate, yCoordinate) in
        let (is_final, endPtsOfContours) =
          match endPtsOfContours with
          | []      -> (false, [])
          | e :: es -> if e = i then (true, es) else (false, endPtsOfContours)
        in
        let tuple = (i + 1, flags, xCoordinates, yCoordinates) in
        if is_final then
          let contour = Alist.to_list (Alist.extend pointacc point) in
          aux Alist.empty (Alist.extend contouracc contour) endPtsOfContours tuple
        else
          aux (Alist.extend pointacc point) contouracc endPtsOfContours tuple

    | _ ->
        assert false
  in
  aux Alist.empty Alist.empty endPtsOfContours (0, flags, xCoordinates, yCoordinates)


let d_simple_glyph (numberOfContours : int) : ttf_simple_glyph_description decoder =
  let open DecodeOperation in
  if numberOfContours = 0 then
    return []
  else
    d_end_points numberOfContours >>= fun endPtsOfContours ->
    (* `num_points`: the total number of points. *)
    let num_points =
      match Alist.chop_last endPtsOfContours with
      | None         -> assert false
      | Some((_, e)) -> e + 1
    in
    let endPtsOfContours = Alist.to_list endPtsOfContours in
    d_uint16 >>= fun instructionLength ->
    d_skip instructionLength >>= fun () ->
    d_flags num_points >>= fun flagacc ->
    let flags = Alist.to_list flagacc in
    d_x_coordinates flags >>= fun xCoordinates ->
    d_y_coordinates flags >>= fun yCoordinates ->
    return (combine endPtsOfContours num_points flags xCoordinates yCoordinates)


type component_flag = {
  arg_1_and_2_are_words    : bool;
  args_are_xy_values       : bool;
  round_xy_to_grid         : bool;
  we_have_a_scale          : bool;
  we_have_an_x_and_y_scale : bool;
  we_have_a_two_by_two     : bool;
  we_have_instructions     : bool;
  use_my_metrics           : bool;
}


let d_component_flag : (bool * component_flag) decoder =
  d_uint16 >>= fun twobytes ->
  let more_components = (twobytes land 32 > 0) in
  let cflag =
    {
      arg_1_and_2_are_words    = (twobytes land 1 > 0);
      args_are_xy_values       = (twobytes land 2 > 0);
      round_xy_to_grid         = (twobytes land 4 > 0);
      we_have_a_scale          = (twobytes land 8 > 0);
      we_have_an_x_and_y_scale = (twobytes land 64 > 0);
      we_have_a_two_by_two     = (twobytes land 128 > 0);
      we_have_instructions     = (twobytes land 256 > 0);
      use_my_metrics           = (twobytes land 512 > 0);
    }
  in
  return (more_components, cflag)


let d_composite_glyph : ttf_composite_glyph_description decoder =
  let open DecodeOperation in
  let rec aux acc =
    d_component_flag >>= fun (more_components, cflags) ->
    d_uint16 >>= fun glyphIndex ->
    let dec = if cflags.arg_1_and_2_are_words then d_int16 else d_int8 in
    dec >>= fun argument1 ->
    dec >>= fun argument2 ->
    begin
      if cflags.we_have_a_scale then
        d_f2dot14 >>= fun scale ->
        return @@ Some{a = scale; b = 0.; c = 0.; d = scale}
      else if cflags.we_have_an_x_and_y_scale then
        d_f2dot14 >>= fun xscale ->
        d_f2dot14 >>= fun yscale ->
        return @@ Some{a = xscale; b = 0.; c = 0.; d = yscale}
      else if cflags.we_have_a_two_by_two then
        d_f2dot14 >>= fun a ->
        d_f2dot14 >>= fun b ->
        d_f2dot14 >>= fun c ->
        d_f2dot14 >>= fun d ->
        return @@ Some{a = a; b = b; c = c; d = d}
      else
        return None
    end >>= fun linear_transform ->
    let v =
      if cflags.args_are_xy_values then
        Vector(argument1, argument2)
      else
        Matching(argument1, argument2)
    in
    let acc = Alist.extend acc (glyphIndex, v, linear_transform) in
    if more_components then
      aux acc
    else
      return @@ Alist.to_list acc
  in
  aux Alist.empty


let d_glyf =
  let open DecodeOperation in
  (* The position is set to the beginning of a glyph. See 5.3.3.1 *)
  d_int16 >>= fun numberOfContours ->
  d_int16 >>= fun xMin ->
  d_int16 >>= fun yMin ->
  d_int16 >>= fun xMax ->
  d_int16 >>= fun yMax ->
  let bbox = { x_min = xMin; y_min = yMin; x_max = xMax; y_max = yMax } in
  if numberOfContours < -1 then
    err @@ Error.InvalidCompositeFormat(numberOfContours)
  else if numberOfContours = -1 then
    d_composite_glyph >>= fun components ->
    return (TtfCompositeGlyph(components), bbox)
  else
    d_simple_glyph numberOfContours >>= fun contours ->
    return (TtfSimpleGlyph(contours), bbox)


let glyf (ttf : ttf_source) (TtfGlyphLocation(reloffset) : ttf_glyph_location) : (ttf_glyph_description * bounding_box) ok =
  let open ResultMonad in
  let common = ttf.ttf_common in
  seek_required_table common.table_directory Value.Tag.table_glyf >>= fun (offset, _length) ->
  d_glyf |> DecodeOperation.run common.core (offset + reloffset)


let d_cff_header : cff_header decoder =
  d_uint8              >>= fun major ->
  d_uint8              >>= fun minor ->
  d_uint8              >>= fun hdrSize ->
  d_offsize            >>= fun offSizeGlobal ->
  d_skip (hdrSize - 4) >>= fun () ->
  return {
    major   = major;
    minor   = minor;
    hdrSize = hdrSize;
    offSize = offSizeGlobal;
  }


let d_charstring_data (len : int) : charstring_data decoder =
  let open DecodeOperation in
  current >>= fun offset ->
  seek (offset + len) >>= fun () ->
  return (CharStringData(offset, len))


let fetch_cff_first (cff : cff_source) : cff_first ok =
  let open ResultMonad in
  seek_required_table cff.cff_common.table_directory Tag.table_cff >>= fun (offset_CFF, _length) ->
  let dec =
    let open DecodeOperation in
    (* Header *)
    d_cff_header >>= fun header ->

    (* Name INDEX (which should contain only one element) *)
    d_index_singleton d_bytes >>= fun name ->

    (* Top DICT INDEX (which should contain only one DICT) *)
    d_index_singleton d_dict >>= fun dictmap ->

    (* String INDEX *)
    d_index d_bytes >>= fun string_index ->

    (* Global Subr INDEX *)
    d_index d_charstring_data >>= fun gsubr_index ->

    return {
      cff_header   = header;
      cff_name     = name;
      top_dict     = dictmap;
      string_index = Array.of_list string_index;
      gsubr_index  = Array.of_list gsubr_index;
      offset_CFF   = offset_CFF;
    }
  in
  dec |> DecodeOperation.run cff.cff_common.core offset_CFF


let get_integer_opt dict key =
  let open ResultMonad in
  match dict |> DictMap.find_opt key with
  | Some(Integer(i) :: []) -> return @@ Some(i)
  | Some(Real(fl) :: [])   -> return @@ Some(int_of_float fl)
  | Some(_)                -> err Error.NotAnIntegerInDict
  | None                   -> return None


let get_integer_with_default dict key default =
  let open ResultMonad in
  get_integer_opt dict key >>= function
  | Some(i) -> return i
  | None    -> return default


let get_integer dict key =
  let open ResultMonad in
  get_integer_opt dict key >>= function
  | Some(i) -> return i
  | None    -> err @@ Error.RequiredKeyNotFound


let get_real_with_default dictmap key dflt =
  let open ResultMonad in
  match DictMap.find_opt key dictmap with
  | Some(Real(r) :: []) -> return r
  | Some(_)             -> err Error.NotARealInDict
  | None                -> return dflt


let get_integer_pair_opt dictmap key =
  let open ResultMonad in
  match DictMap.find_opt key dictmap with
  | Some(Integer(i1) :: Integer(i2) :: []) -> return (Some(i1, i2))
  | Some(Integer(i1) :: Real(fl2) :: [])   -> return (Some(i1, int_of_float fl2))
  | Some(Real(fl1) :: Integer(i2) :: [])   -> return (Some(int_of_float fl1, i2))
  | Some(Real(fl1) :: Real(fl2) :: [])     -> return (Some(int_of_float fl1, int_of_float fl2))
  | Some(_)                                -> err @@ Error.NotAnIntegerPairInDict
  | None                                   -> return None


let get_iquad_opt dict key default =
  let open ResultMonad in
  match dict |> DictMap.find_opt key with
  | Some(Integer(i1) :: Integer(i2) :: Integer(i3) :: Integer(i4) :: []) -> return (i1, i2, i3, i4)
  | Some(_)                                                              -> err Error.NotAQuadrupleInDict
  | None                                                                 -> return default


let get_ros dictmap key =
  let open ResultMonad in
  match dictmap |> DictMap.find_opt key with
  | Some(Integer(sid1) :: Integer(sid2) :: Integer(i) :: []) -> return (sid1, sid2, i)
  | Some(_)                                                  -> err Error.InvalidRos
  | None                                                     -> err Error.InvalidRos


let get_boolean_with_default dict key dflt =
  let open ResultMonad in
  get_integer_with_default dict key (if dflt then 1 else 0) >>= fun i ->
  return (i <> 0)


let get_string string_index sid =
  let open ResultMonad in
  let nStdString = 391 in
  if sid < nStdString then
    failwith "a standard string; remains to be supported."
  else
    try return string_index.(sid - nStdString) with
    | Invalid_argument(_) -> err @@ Error.SidOutOfBounds(sid)


let fetch_number_of_glyphs (cff : cff_source) (offset_CharString_INDEX : offset) : int ok =
  d_uint16 |> DecodeOperation.run cff.cff_common.core offset_CharString_INDEX


let d_single_private (size_private : int) : single_private decoder =
  let open DecodeOperation in
  let ( !@ ) = transform_result in
  current >>= fun offset_private ->
  d_dict size_private >>= fun dict_private ->
  !@ (get_integer_opt          dict_private (ShortKey(19)))   >>= fun selfoffset_lsubrs_opt ->
  !@ (get_integer_with_default dict_private (ShortKey(20)) 0) >>= fun default_width_x ->
  !@ (get_integer_with_default dict_private (ShortKey(21)) 0) >>= fun nominal_width_x ->

  (* Local Subr INDEX *)
  begin
    match selfoffset_lsubrs_opt with
    | None ->
        return []

    | Some(selfoffset_lsubrs) ->
        let offset_lsubrs = offset_private + selfoffset_lsubrs in
        seek offset_lsubrs >>= fun () ->
        d_index d_charstring_data
  end >>= fun lsubr_index ->
  return { default_width_x; nominal_width_x; local_subr_index = Array.of_list lsubr_index }


let fetch_single_private (cff : cff_source) (offset_CFF : offset) (dict : dict) : single_private ok =
  (* Private DICT *)
  let open ResultMonad in
  get_integer_pair_opt dict (ShortKey(18)) >>= function
  | None ->
      err Error.NoPrivateDict

  | Some(size_private, reloffset_private) ->
      let offset_private = offset_CFF + reloffset_private in
      let dec = d_single_private size_private in
      dec |> DecodeOperation.run cff.cff_common.core offset_private


let fetch_fdarray (cff : cff_source) (offset_CFF : offset) (offset_FDArray : offset) : fdarray ok =
  let dec =
    let open DecodeOperation in
    d_index d_dict >>= fun dicts ->
    dicts |> mapM (fun dict ->
      transform_result @@ fetch_single_private cff offset_CFF dict
    ) >>= fun single_privates ->
    return @@ Array.of_list single_privates
  in
  dec |> DecodeOperation.run cff.cff_common.core offset_FDArray


let d_fdselect_format_0 (nGlyphs : int) : fdselect decoder =
  let open DecodeOperation in
  let idx = Array.make nGlyphs 0 in
  let rec aux i =
    if i >= nGlyphs then
      return (FDSelectFormat0(idx))
    else
      begin
        d_uint8 >>= fun v ->
        idx.(i) <- v;
        aux (i + 1)
      end

  in
  aux 0


let d_fdselect_format_3 : fdselect decoder =
  let open DecodeOperation in
  let rec aux num i acc =
    if i >= num then
      d_uint16 >>= fun gid_sentinel ->
      return (FDSelectFormat3(Alist.to_list acc, gid_sentinel))
    else
      d_uint16 >>= fun gid ->
      d_uint8 >>= fun v ->
      aux num (i + 1) (Alist.extend acc (gid, v))
  in
  d_uint16 >>= fun nRanges ->
  aux nRanges 0 Alist.empty


let fetch_fdselect (cff : cff_source) (nGlyphs : int) (offset_FDSelect : offset) : fdselect ok =
  let open DecodeOperation in
  let dec =
    d_uint8 >>= function
    | 0 -> d_fdselect_format_0 nGlyphs
    | 3 -> d_fdselect_format_3
    | n -> err @@ Error.UnknownFdselectFormat(n)
  in
  dec |> DecodeOperation.run cff.cff_common.core offset_FDSelect


let make_cff_info (cff : cff_source) : (cff_top_dict * charstring_info) ok =
  let open ResultMonad in
  fetch_cff_first cff >>= fun cff_first ->
  let font_name    = cff_first.cff_name in
  let top_dict     = cff_first.top_dict in
  let string_index = cff_first.string_index in
  let gsubr_index  = cff_first.gsubr_index in
  let offset_CFF   = cff_first.offset_CFF in
  get_boolean_with_default top_dict (LongKey(1)) false  >>= fun is_fixed_pitch ->
  get_integer_with_default top_dict (LongKey(2)) 0      >>= fun italic_angle ->
  get_integer_with_default top_dict (LongKey(3)) (-100) >>= fun underline_position ->
  get_integer_with_default top_dict (LongKey(4)) 50     >>= fun underline_thickness ->
  get_integer_with_default top_dict (LongKey(5)) 0      >>= fun paint_type ->
  get_integer_with_default top_dict (LongKey(6)) 2      >>= fun charstring_type ->
  if charstring_type <> 2 then
    err @@ Error.UnknownCharstringType(charstring_type)
  else
    get_iquad_opt            top_dict (ShortKey(5)) (0, 0, 0, 0) >>= fun font_bbox ->
    get_integer_with_default top_dict (LongKey(8) ) 0            >>= fun stroke_width ->
    get_integer              top_dict (ShortKey(17))             >>= fun reloffset_CharString_INDEX ->
    let offset_CharString_INDEX = offset_CFF + reloffset_CharString_INDEX in
    fetch_number_of_glyphs cff offset_CharString_INDEX >>= fun number_of_glyphs ->
    begin
      if DictMap.mem (LongKey(30)) top_dict then
      (* If the font is a CIDFont *)
        get_ros                  top_dict (LongKey(30))      >>= fun (sid_registry, sid_ordering, supplement) ->
        get_real_with_default    top_dict (LongKey(31)) 0.   >>= fun cid_font_version ->
        get_integer_with_default top_dict (LongKey(32)) 0    >>= fun cid_font_revision ->
        get_integer_with_default top_dict (LongKey(33)) 0    >>= fun cid_font_type ->
        get_integer_with_default top_dict (LongKey(34)) 8720 >>= fun cid_count ->
        get_integer              top_dict (LongKey(36))      >>= fun reloffset_FDArray ->
        get_integer              top_dict (LongKey(37))      >>= fun reloffset_FDSelect ->
        let offset_FDArray = offset_CFF + reloffset_FDArray in
        let offset_FDSelect = offset_CFF + reloffset_FDSelect in
        get_string string_index sid_registry >>= fun registry ->
        get_string string_index sid_ordering >>= fun ordering ->
        fetch_fdarray cff offset_CFF offset_FDArray >>= fun fdarray ->
        fetch_fdselect cff number_of_glyphs offset_FDSelect >>= fun fdselect ->
        return (Some{
          registry; ordering; supplement;
          cid_font_version;
          cid_font_revision;
          cid_font_type;
          cid_count;
        }, FontDicts(fdarray, fdselect))
    else
    (* If the font is not a CIDFont *)
      fetch_single_private cff offset_CFF top_dict >>= fun singlepriv ->
      return (None, SinglePrivate(singlepriv))
  end >>= fun (cid_info, private_info) ->
  let cff_top =
    {
      font_name;
      is_fixed_pitch;
      italic_angle;
      underline_position;
      underline_thickness;
      paint_type;
      font_bbox;
      stroke_width;
      cid_info;
      number_of_glyphs;
    }
  in
  let charstring_info = (gsubr_index, private_info, offset_CharString_INDEX) in
  return (cff_top, charstring_info)


let charstring (cff : cff_source) (_gid : glyph_id) : ((int option * parsed_charstring list) option) ok =
  let open ResultMonad in
  make_cff_info cff >>= fun (_, charstring_info) ->
  let (_gsubr_index, _private_info, _offset_CharString_INDEX) = charstring_info in
  failwith "TODO: charstring"



module ForTest = struct
  type 'a decoder = 'a DecodeOperation.decoder
  let run s d = DecodeOperation.run { data = s; max = String.length s - 1 } 0 d
  let d_glyf = d_glyf
end
