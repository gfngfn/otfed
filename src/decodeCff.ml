
open Basic
open Value
open DecodeBasic
open DecodeOperation.Open
open DecodeOperationCff
open Intermediate.Cff


module Maxp = DecodeCffMaxp


let max_depth_limit = 10
  (* See "The Type 2 Charstring Format", Appendix B. *)


let d_cff_header : cff_header decoder =
  let open DecodeOperation in
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


let d_charstring_data (length : int) : charstring_data decoder =
  let open DecodeOperation in
  current >>= fun offset ->
  d_skip length >>= fun () ->
  return (CharStringData(offset, length))


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
  | None    -> err @@ Error.RequiredKeyNotFound(key)


let get_real_with_default dictmap key default =
  let open ResultMonad in
  match DictMap.find_opt key dictmap with
  | Some(Real(r) :: []) -> return r
  | Some(_)             -> err Error.NotARealInDict
  | None                -> return default


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
  if sid < 0 then
    err @@ Error.SidOutOfBounds(sid)
  else if sid < nStdString then
    match CffStandardString.get sid with
    | None    -> assert false
    | Some(s) -> return s
  else
    try return string_index.(sid - nStdString) with
    | Invalid_argument(_) ->
        err @@ Error.SidOutOfBounds(sid)


let get_string_opt string_index sid_opt =
  let open ResultMonad in
  match sid_opt with
  | None ->
      return None

  | Some(sid) ->
      get_string string_index sid >>= fun s ->
      return @@ Some(s)


let fetch_number_of_glyphs (core : common_source_core) ~(offset_CharString_INDEX : offset) : int ok =
  let open DecodeOperation in
  d_uint16 |> run core offset_CharString_INDEX


let d_single_private (size_private : int) : single_private decoder =
  let open DecodeOperation in
  current >>= fun offset_private ->
  d_dict size_private >>= fun dict_private ->
  transform_result @@ get_integer_opt          dict_private (ShortKey(19))   >>= fun self_offset_lsubrs_opt ->
  transform_result @@ get_integer_with_default dict_private (ShortKey(20)) 0 >>= fun default_width_x ->
  transform_result @@ get_integer_with_default dict_private (ShortKey(21)) 0 >>= fun nominal_width_x ->

  (* Local Subr INDEX *)
  begin
    match self_offset_lsubrs_opt with
    | None ->
        return []

    | Some(self_offset_lsubrs) ->
        let offset_lsubrs = offset_private + self_offset_lsubrs in
        seek offset_lsubrs >>= fun () ->
        d_index d_charstring_data
  end >>= fun lsubr_index ->
  return { default_width_x; nominal_width_x; local_subr_index = Array.of_list lsubr_index }


let fetch_single_private (core : common_source_core) ~(offset_CFF : offset) (dict : dict) : single_private ok =
  (* Private DICT *)
  let open ResultMonad in
  get_integer_pair_opt dict (ShortKey(18)) >>= function
  | None ->
      err Error.NoPrivateDict

  | Some(size_private, zero_offset_private) ->
      let offset_private = offset_CFF + zero_offset_private in
      let dec = d_single_private size_private in
      dec |> DecodeOperation.run core offset_private


let fetch_fdarray (core : common_source_core) ~(offset_CFF : offset) ~(offset_FDArray : offset) : fdarray ok =
  let dec =
    let open DecodeOperation in
    d_index d_dict >>= fun dicts ->
    dicts |> mapM (fun dict ->
      transform_result @@ fetch_single_private core ~offset_CFF dict
    ) >>= fun single_privates ->
    return @@ Array.of_list single_privates
  in
  dec |> DecodeOperation.run core offset_FDArray


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


let fetch_fdselect (core : common_source_core) ~(number_of_glyphs : int) ~(offset_FDSelect : offset) : fdselect ok =
  let open DecodeOperation in
  let dec =
    d_uint8 >>= function
    | 0 -> d_fdselect_format_0 number_of_glyphs
    | 3 -> d_fdselect_format_3
    | n -> err @@ Error.UnknownFdselectFormat(n)
  in
  dec |> DecodeOperation.run core offset_FDSelect


type cff_first = {
  cff_first_header       : cff_header;
  cff_first_name         : string;           (* singleton Name INDEX *)
  cff_first_top_dict     : dict;             (* singleton Top DICT INDEX *)
  cff_first_string_index : string_index;     (* String INDEX [CFF p.17, Section 10] *)
  cff_first_gsubr_index  : subroutine_index;
}


let fetch_cff_first ~(offset_CFF : offset) (core : common_source_core) : cff_first ok =
  let dec =
    let open DecodeOperation in
    (* Header *)
    d_cff_header >>= fun header ->

    (* Name INDEX (which should contain only one element) *)
    d_index_singleton d_bytes >>= fun name ->

    (* Top DICT INDEX (which should contain only one DICT) *)
    d_index_singleton d_dict >>= fun top_dict ->

    (* String INDEX *)
    d_index d_bytes >>= fun string_index ->

    (* Global Subr INDEX *)
    d_index d_charstring_data >>= fun gsubr_index ->

    return {
      cff_first_header       = header;
      cff_first_name         = name;
      cff_first_top_dict     = top_dict;
      cff_first_string_index = Array.of_list string_index;
      cff_first_gsubr_index  = Array.of_list gsubr_index;
    }
  in
  dec |> DecodeOperation.run core offset_CFF


let fetch_cff_specific (core : common_source_core) (table_directory : table_directory) : cff_specific_source ok =
  let open ResultMonad in
  DecodeOperation.seek_required_table table_directory Tag.table_cff >>= fun (offset_CFF, _length) ->
  fetch_cff_first ~offset_CFF core >>= fun cff_first ->
  let font_name    = cff_first.cff_first_name in
  let top_dict     = cff_first.cff_first_top_dict in
  let string_index = cff_first.cff_first_string_index in
  let gsubr_index  = cff_first.cff_first_gsubr_index in
  get_integer_opt          top_dict (ShortKey(0))       >>= fun sid_version_opt ->
  get_integer_opt          top_dict (ShortKey(1))       >>= fun sid_notice_opt ->
  get_integer_opt          top_dict (LongKey(0))        >>= fun sid_copyright_opt ->
  get_integer_opt          top_dict (ShortKey(2))       >>= fun sid_full_name_opt ->
  get_integer_opt          top_dict (ShortKey(3))       >>= fun sid_family_name_opt ->
  get_integer_opt          top_dict (ShortKey(4))       >>= fun sid_weight_opt ->
  get_boolean_with_default top_dict (LongKey(1)) false  >>= fun is_fixed_pitch ->
  get_integer_with_default top_dict (LongKey(2)) 0      >>= fun italic_angle ->
  get_integer_with_default top_dict (LongKey(3)) (-100) >>= fun underline_position ->
  get_integer_with_default top_dict (LongKey(4)) 50     >>= fun underline_thickness ->
  get_integer_with_default top_dict (LongKey(5)) 0      >>= fun paint_type ->
  get_integer_with_default top_dict (LongKey(6)) 2      >>= fun charstring_type ->
  if charstring_type <> 2 then
    err @@ Error.UnknownCharstringType(charstring_type)
  else
    get_iquad_opt            top_dict (ShortKey(5)) (0, 0, 0, 0) >>= fun font_bbox_quad ->
    get_integer_with_default top_dict (LongKey(8) ) 0            >>= fun stroke_width ->
    get_integer_with_default top_dict (ShortKey(15)) 0           >>= fun charset_number ->
    let font_bbox =
      let (x_min, y_min, x_max, y_max) = font_bbox_quad in
      Value.{ x_min; y_min; x_max; y_max }
    in
    let charset =
      match charset_number with
      | 0           -> PredefinedCharset(IsoAdobeCharset)
      | 1           -> PredefinedCharset(ExpertCharset)
      | 2           -> PredefinedCharset(ExpertCharset)
      | zero_offset -> CharsetData(offset_CFF + zero_offset)
    in
    get_integer              top_dict (ShortKey(17))             >>= fun zero_offset_CharString_INDEX ->
    let offset_CharString_INDEX = offset_CFF + zero_offset_CharString_INDEX in
    fetch_number_of_glyphs core ~offset_CharString_INDEX >>= fun number_of_glyphs ->
    get_string_opt string_index sid_version_opt     >>= fun version ->
    get_string_opt string_index sid_notice_opt      >>= fun notice ->
    get_string_opt string_index sid_copyright_opt   >>= fun copyright ->
    get_string_opt string_index sid_full_name_opt   >>= fun full_name ->
    get_string_opt string_index sid_family_name_opt >>= fun family_name ->
    get_string_opt string_index sid_weight_opt      >>= fun weight ->
    begin
      if DictMap.mem (LongKey(30)) top_dict then
      (* If the font is a CIDFont *)
        get_ros                  top_dict (LongKey(30))      >>= fun (sid_registry, sid_ordering, supplement) ->
        get_real_with_default    top_dict (LongKey(31)) 0.   >>= fun cid_font_version ->
        get_integer_with_default top_dict (LongKey(32)) 0    >>= fun cid_font_revision ->
        get_integer_with_default top_dict (LongKey(33)) 0    >>= fun cid_font_type ->
        get_integer_with_default top_dict (LongKey(34)) 8720 >>= fun cid_count ->
        get_integer              top_dict (LongKey(36))      >>= fun zero_offset_FDArray ->
        get_integer              top_dict (LongKey(37))      >>= fun zero_offset_FDSelect ->
        let offset_FDArray = offset_CFF + zero_offset_FDArray in
        let offset_FDSelect = offset_CFF + zero_offset_FDSelect in
        get_string string_index sid_registry >>= fun registry ->
        get_string string_index sid_ordering >>= fun ordering ->
        fetch_fdarray core ~offset_CFF ~offset_FDArray >>= fun fdarray ->
        fetch_fdselect core ~number_of_glyphs ~offset_FDSelect >>= fun fdselect ->
        return (Some(Intermediate.Cff.{
          registry; ordering; supplement;
          cid_font_version;
          cid_font_revision;
          cid_font_type;
          cid_count;
        }), FontDicts(fdarray, fdselect))
      else
      (* If the font is not a CIDFont *)
        fetch_single_private core ~offset_CFF top_dict >>= fun singlepriv ->
        return (None, SinglePrivate(singlepriv))
  end >>= fun (cid_info, private_info) ->
  let cff_top_dict =
    Intermediate.Cff.{
      font_name;
      version;
      notice;
      copyright;
      full_name;
      family_name;
      weight;
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
  let charstring_info =
    {
      gsubr_index;
      private_info;
      charset;
      string_index;
      offset_CharString_INDEX;
    }
  in
  return { cff_top_dict; charstring_info }


let top_dict (cff : cff_source) : Intermediate.Cff.top_dict ok =
  let open ResultMonad in
  return cff.cff_specific.cff_top_dict


let rec d_access_charset_sub (d_num_left : int decoder) (i : int) : int decoder =
  let open DecodeOperation in
  d_uint16   >>= fun sid_first ->
  d_num_left >>= fun num_left ->
  if i < num_left + 1 then
    return @@ sid_first + i
  else
    d_access_charset_sub d_num_left (i - num_left - 1)


let d_access_charset (string_index : string_index) (gid : glyph_id) : (string option) decoder =
  (* Here we can assume `0 <= gid <= numGlyphs`. *)
  let open DecodeOperation in
  if gid = 0 then
    return @@ Some(".notdef")
  else
    d_uint8 >>= fun format ->
    begin
      match format with
      | 0 ->
          d_skip ((gid - 1) * 2) >>= fun () ->
          d_uint16

      | 1 ->
          d_access_charset_sub d_uint8 (gid - 1)

      | 2 ->
          d_access_charset_sub d_uint16 (gid - 1)

      | _ ->
          err @@ Error.UnknownFormatNumber(format)
    end >>= fun sid ->
    transform_result @@ begin
      let open ResultMonad in
      match get_string string_index sid with
      | Error(_) -> return @@ None
      | Ok(s)    -> return @@ Some(s)
    end


let access_charset (cff : cff_source) (gid : glyph_id) : (string option) ok =
  let open ResultMonad in
  let { charstring_info; _ } = cff.cff_specific in
  let { string_index; charset; _ } = charstring_info in
  if gid < 0 || gid >= cff.cff_common.num_glyphs then
    return None
  else
    match charset with
    | PredefinedCharset(_) ->
        return None

    | CharsetData(offset_charset) ->
        d_access_charset string_index gid |> DecodeOperation.run cff.cff_common.core offset_charset


let fetch_charstring_data (cff : cff_source) (offset_CharString_INDEX : offset) (gid : glyph_id) =
  let open DecodeOperation in
  let dec = d_index_access d_charstring_data gid in
  dec |> run cff.cff_common.core offset_CharString_INDEX


let select_fd_index (fdselect : fdselect) (gid : glyph_id) : fdindex ok =
  let open ResultMonad in
  match fdselect with
  | FDSelectFormat0(arr) ->
      begin
        try return arr.(gid) with
        | Invalid_argument(_) ->
            err @@ Error.FdselectOutOfBounds(gid)
      end

  | FDSelectFormat3(pairs, gid_sentinel) ->
      if gid >= gid_sentinel then
        err @@ Error.FdselectOutOfBounds(gid)
      else
        let opt =
          pairs |> List.fold_left (fun opt (gidc, fdi) ->
            if gidc <= gid then
              Some(fdi)
            else
              opt
          ) None
        in
        begin
          match opt with
          | None      -> err @@ Error.FdselectOutOfBounds(gid)
          | Some(fdi) -> return fdi
        end


let select_local_subr_index (private_info : private_info) (gid : glyph_id) : subroutine_index ok =
  let open ResultMonad in
  match private_info with
  | SinglePrivate(singlepriv) ->
      return singlepriv.local_subr_index

  | FontDicts(fdarray, fdselect) ->
      select_fd_index fdselect gid >>= fun fdindex ->
      try
        let singlepriv = fdarray.(fdindex) in
        return singlepriv.local_subr_index
      with
      | Invalid_argument(_) ->
          err @@ Error.FdindexOutOfBounds(fdindex)


module IntSet = Set.Make(Int)


type charstring_constant = {
  gsubr_index : subroutine_index;
  lsubr_index : subroutine_index;
}

type width_state =
  | LookingForWidth
  | WidthDecided of int option

type stack = int ImmutStack.t

type charstring_lexing_state = {
  remaining   : int;
  num_args    : int;
  num_stems   : int;
}

type charstring_state = {
  lexing      : charstring_lexing_state;
  width       : width_state;
  stack       : stack;
  used_gsubrs : IntSet.t;
  used_lsubrs : IntSet.t;
}


let d_stem_argument (num_stems : int) : (int * Intermediate.Cff.stem_argument) decoder =
  let open DecodeOperation in
  let arglen =
    if num_stems mod 8 = 0 then
      num_stems / 8
    else
      num_stems / 8 + 1
  in
  d_bytes arglen >>= fun arg ->
  return (arglen, arg)


let d_charstring_token (lstate : charstring_lexing_state) : (charstring_lexing_state * Intermediate.Cff.charstring_token) decoder =
  let open DecodeOperation in
  let open Intermediate.Cff in
  let num_args = lstate.num_args in
  let num_stems = lstate.num_stems in
  let return_simple (step, cselem) =
    return ({ lstate with remaining = lstate.remaining - step }, cselem)
  in
  let return_argument (step, cselem) =
    let lstate =
      { lstate with
        num_args  = num_args + 1;
        remaining = lstate.remaining - step
      }
    in
    return (lstate, cselem)
  in
  let return_flushing_operator (step, cselem) =
    let lstate =
      { lstate with
        num_args  = 0;
        remaining = lstate.remaining - step
      }
    in
    return (lstate, cselem)
  in
  let return_subroutine_operator cselem =
    let lstate =
      { lstate with
        num_args  = num_args - 1;
        remaining = lstate.remaining - 1
      }
    in
    return (lstate, cselem)
  in
  let return_stem (step, cselem) =
    let lstate =
      {
        num_args  = 0;
        num_stems = num_stems + num_args / 2;
        remaining = lstate.remaining - step
      }
    in
    return (lstate, cselem)
  in
  (* `num_args` may be an odd number, but it is due to the width value *)
  d_uint8 >>= function

  | 1  -> return_stem (1, OpHStem)
  | 3  -> return_stem (1, OpVStem)
  | 18 -> return_stem (1, OpHStemHM)
  | 23 -> return_stem (1, OpVStemHM)

  | 4  -> return_flushing_operator (1, OpVMoveTo)
  | 5  -> return_flushing_operator (1, OpRLineTo)
  | 6  -> return_flushing_operator (1, OpHLineTo)
  | 7  -> return_flushing_operator (1, OpVLineTo)
  | 8  -> return_flushing_operator (1, OpRRCurveTo)

  | 10 -> return_subroutine_operator OpCallSubr
  | 29 -> return_subroutine_operator OpCallGSubr

  | 11 ->
      return_simple (1, OpReturn)

  | 12 ->
      begin
        d_uint8 >>= fun b1 ->
        match b1 with
        | (3 | 4 | 5 | 9 | 10 | 11 | 12 | 14 | 15) ->
            err @@ Error.Unsupported(CharstringArithmeticOperator(b1))

        | 34 -> return_flushing_operator (2, OpHFlex)
        | 35 -> return_flushing_operator (2, OpFlex)
        | 36 -> return_flushing_operator (2, OpHFlex1)
        | 37 -> return_flushing_operator (2, OpFlex1)

        | _ ->
            err @@ Error.UnknownCharstringLongOperator(b1)
      end

  | 14 -> return_flushing_operator (1, OpEndChar)

  | 19 -> (* `hintmask` operator *)
      d_stem_argument (num_stems + num_args / 2) >>= fun (step, bits) ->
      return_stem (1 + step, OpHintMask(bits))

  | 20 -> (* `cntrmask` operator *)
      d_stem_argument (num_stems + num_args / 2) >>= fun (step, bits) ->
      return_stem (1 + step, OpCntrMask(bits))

  | 21 -> return_flushing_operator (1, OpRMoveTo)
  | 22 -> return_flushing_operator (1, OpHMoveTo)
  | 24 -> return_flushing_operator (1, OpRCurveLine)
  | 25 -> return_flushing_operator (1, OpRLineCurve)
  | 26 -> return_flushing_operator (1, OpVVCurveTo)
  | 27 -> return_flushing_operator (1, OpHHCurveTo)

  | 28 ->
      d_twoscompl2 >>= fun ret ->
      return_argument (3, ArgumentInteger(ret))

  | 30 -> return_flushing_operator (1, OpVHCurveTo)
  | 31 -> return_flushing_operator (1, OpHVCurveTo)

  | b0 when b0 |> is_in_range ~lower:32 ~upper:246 ->
      return_argument (1, ArgumentInteger(b0 - 139))

  | b0 when b0 |> is_in_range ~lower:247 ~upper:250 ->
      d_uint8 >>= fun b1 ->
      return_argument (2, ArgumentInteger((b0 - 247) * 256 + b1 + 108))

  | b0 when b0 |> is_in_range ~lower:251 ~upper:254 ->
      d_uint8 >>= fun b1 ->
      return_argument (2, ArgumentInteger(- (b0 - 251) * 256 - b1 - 108))

  | 255 ->
      d_twoscompl2 >>= fun ret1 ->
      d_twoscompl2 >>= fun ret2 ->
      let ret = float_of_int ret1 +. (float_of_int ret2) /. (float_of_int (1 lsl 16)) in
      return_argument (5, ArgumentReal(ret))

  | n ->
      err @@ Error.UnknownCharstringToken(n)


let pop_mandatory (stack : stack) : (stack * int) decoder =
  let open DecodeOperation in
  match ImmutStack.pop stack with
  | None       -> err Error.CharstringStackUnderflow
  | Some(pair) -> return pair


let pop_opt (stack : stack) : stack * int option =
  match ImmutStack.pop stack with
  | None             -> (stack, None)
  | Some((stack, v)) -> (stack, Some(v))


let pop2_opt (stack : stack) : (stack * (int * int)) option =
  let ( >>= ) = Option.bind in
  ImmutStack.pop stack >>= fun (stack, y) ->
  ImmutStack.pop stack >>= fun (stack, x) ->
  Some((stack, (x, y)))


let pop4_opt (stack : stack) : (stack * (int * int * int * int)) option =
  let ( >>= ) = Option.bind in
  ImmutStack.pop stack >>= fun (stack, d4) ->
  ImmutStack.pop stack >>= fun (stack, d3) ->
  ImmutStack.pop stack >>= fun (stack, d2) ->
  ImmutStack.pop stack >>= fun (stack, d1) ->
  Some((stack, (d1, d2, d3, d4)))


let pop6_opt (stack : stack) : (stack * (int * int * int * int * int * int)) option =
  let ( >>= ) = Option.bind in
  ImmutStack.pop stack >>= fun (stack, d6) ->
  ImmutStack.pop stack >>= fun (stack, d5) ->
  ImmutStack.pop stack >>= fun (stack, d4) ->
  ImmutStack.pop stack >>= fun (stack, d3) ->
  ImmutStack.pop stack >>= fun (stack, d2) ->
  ImmutStack.pop stack >>= fun (stack, d1) ->
  Some((stack, (d1, d2, d3, d4, d5, d6)))


let pop_iter (popf : stack -> (stack * 'a) option) (stack : stack) : stack * 'a list =
  let rec aux stack acc =
    match popf stack with
    | None               -> (stack, acc)  (* returns in the forward direction *)
    | Some((stack, ret)) -> aux stack (ret :: acc)
  in
  aux stack []


let pop_opt_for_width (width : width_state) (stack : stack) : stack * width_state =
  match width with
  | WidthDecided(_) ->
      (stack, width)

  | LookingForWidth ->
      let (stack, wopt) = pop_opt stack in
      (stack, WidthDecided(wopt))


let make_bezier (dxa, dya, dxb, dyb, dxc, dyc) =
  ((dxa, dya), (dxb, dyb), (dxc, dyc))


let convert_subroutine_number (subr_index : subroutine_index) (i : int) =
  let arrlen = Array.length subr_index in
  let bias =
    if arrlen < 1240 then 107 else
      if arrlen < 33900 then 1131 else
        32768
  in
  bias + i


let access_subroutine (subr_index : subroutine_index) (i : int) : (offset * int * int) ok =
  let open ResultMonad in
  let biased_number = convert_subroutine_number subr_index i in
  try
    let CharStringData(offset, length) = subr_index.(biased_number) in
    return (offset, length, biased_number)
  with
  | Invalid_argument(_) ->
      err @@ Error.SubroutineIndexOutOfBounds{ index = i; biased = biased_number }


let rec parse_progress (cconst : charstring_constant) (cstate : charstring_state) =
  let open DecodeOperation in
  let open Intermediate.Cff in
  d_charstring_token cstate.lexing >>= fun (lstate, cselem) ->
  let cstate = { cstate with lexing = lstate } in
  let stack = cstate.stack in
  match cselem with
  | ArgumentInteger(i) ->
      let stack = stack |> ImmutStack.push i in
      return ({ cstate with stack }, [])

  | ArgumentReal(r) ->
      let stack = stack |> ImmutStack.push (int_of_float r) in
      return ({ cstate with stack }, [])

  | OpHStem ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      begin
        match pairs with
        | [] ->
            err Error.CharstringStackUnderflow

        | (y, dy) :: rest ->
            let (stack, width) = pop_opt_for_width cstate.width stack in
            return ({ cstate with width; stack }, [HStem{ y; dy; rest }])
      end

  | OpVStem ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      begin
        match pairs with
        | [] ->
            err Error.CharstringStackUnderflow

        | (x, dx) :: rest ->
            let (stack, width) = pop_opt_for_width cstate.width stack in
            return ({ cstate with width; stack }, [VStem{ x; dx; rest }])
      end

  | OpVMoveTo ->
      pop_mandatory stack >>= fun (stack, dy1) ->
      let (stack, width) = pop_opt_for_width cstate.width stack in
      return ({ cstate with width; stack }, [VMoveTo{ dy1 }])

  | OpRLineTo ->
      let (stack, cspts) = pop_iter pop2_opt stack in
      return ({ cstate with stack }, [RLineTo(cspts)])

  | OpHLineTo ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      let (stack, firstopt) = pop_opt stack in
      let flats = pairs |> List.map (fun (a, b) -> [a; b]) |> List.concat in
      begin
        match firstopt with
        | None        -> return ({ cstate with stack }, [HLineTo(flats)])
        | Some(first) -> return ({ cstate with stack }, [HLineTo(first :: flats)])
      end

  | OpVLineTo ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      let (stack, firstopt) = pop_opt stack in
      let flats = pairs |> List.map (fun (a, b) -> [a; b]) |> List.concat in
      begin
        match firstopt with
        | None        -> return ({ cstate with stack }, [VLineTo(flats)])
        | Some(first) -> return ({ cstate with stack }, [VLineTo(first :: flats)])
      end

  | OpRRCurveTo ->
      let (stack, tuples) = pop_iter pop6_opt stack in
      let beziers = tuples |> List.map make_bezier in
      return ({ cstate with stack }, [RRCurveTo(beziers)])

  | OpCallSubr ->
      d_subroutine cconst cstate cconst.lsubr_index

  | OpReturn ->
      return (cstate, [])

  | OpEndChar ->
      let (stack, width) = pop_opt_for_width cstate.width stack in
      if ImmutStack.is_empty stack then
        return ({ cstate with width; stack }, [])
      else
        err Error.CharstringStackRemaining

  | OpHStemHM ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      begin
        match pairs with
        | [] ->
            err Error.CharstringStackUnderflow

        | (y, dy) :: rest ->
            let (stack, width) = pop_opt_for_width cstate.width stack in
            return ({ cstate with width; stack }, [HStemHM{ y; dy; rest }])
      end

  | OpHintMask(arg) ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      let (stack, width) = pop_opt_for_width cstate.width stack in
      let cstate = { cstate with width; stack } in
      begin
        match pairs with
        | []              -> return (cstate, [HintMask(arg)])
        | (x, dx) :: rest -> return (cstate, [VStemHM{ x; dx; rest }; HintMask(arg)])
      end

  | OpCntrMask(arg) ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      let (stack, width) = pop_opt_for_width cstate.width stack in
      let cstate = { cstate with width; stack } in
      begin
        match pairs with
        | []              -> return (cstate, [CntrMask(arg)])
        | (x, dx) :: rest -> return (cstate, [VStemHM{ x; dx; rest }; CntrMask(arg)])
      end

  | OpRMoveTo ->
      pop_mandatory stack >>= fun (stack, dy1) ->
      pop_mandatory stack >>= fun (stack, dx1) ->
      let (stack, width) = pop_opt_for_width cstate.width stack in
      return ({ cstate with width; stack }, [RMoveTo{ dv1 = (dx1, dy1) }])

  | OpHMoveTo ->
      pop_mandatory stack >>= fun (stack, dx1) ->
      let (stack, width) = pop_opt_for_width cstate.width stack in
      return ({ cstate with width; stack }, [HMoveTo{ dx1 }])

  | OpVStemHM ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      begin
        match pairs with
        | [] ->
            err Error.CharstringStackUnderflow

        | (x, dx) :: rest ->
            let (stack, width) = pop_opt_for_width cstate.width stack in
            return ({ cstate with width; stack }, [VStemHM{ x; dx; rest }])
      end

  | OpRCurveLine ->
      pop_mandatory stack >>= fun (stack, dyd) ->
      pop_mandatory stack >>= fun (stack, dxd) ->
      let (stack, tuples) = pop_iter pop6_opt stack in
      let beziers = tuples |> List.map make_bezier in
      return ({ cstate with stack }, [ RRCurveTo(beziers); RLineTo([ (dxd, dyd) ]) ])

  | OpRLineCurve ->
      pop_mandatory stack >>= fun (stack, dyd) ->
      pop_mandatory stack >>= fun (stack, dxd) ->
      pop_mandatory stack >>= fun (stack, dyc) ->
      pop_mandatory stack >>= fun (stack, dxc) ->
      pop_mandatory stack >>= fun (stack, dyb) ->
      pop_mandatory stack >>= fun (stack, dxb) ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      return ({ cstate with stack }, [ RLineTo(pairs); RRCurveTo([ ((dxb, dyb), (dxc, dyc), (dxd, dyd)) ]) ])

  | OpVVCurveTo ->
      let (stack, tuples) = pop_iter pop4_opt stack in
      let rest = tuples |> List.map (fun (dya, dxb, dyb, dyc) -> (dya, (dxb, dyb), dyc)) in
      let (stack, dx1_opt) = pop_opt stack in
      return ({ cstate with stack }, [ VVCurveTo{ dx1 = dx1_opt; rest } ])

  | OpHHCurveTo ->
      let (stack, tuples) = pop_iter pop4_opt stack in
      let rest = tuples |> List.map (fun (dxa, dxb, dyb, dxc) -> (dxa, (dxb, dyb), dxc)) in
      let (stack, dy1_opt) = pop_opt stack in
      return ({ cstate with stack }, [ HHCurveTo{ dy1 = dy1_opt; rest } ])

  | OpCallGSubr ->
      d_subroutine cconst cstate cconst.gsubr_index

  | OpVHCurveTo ->
      begin
        if ImmutStack.size stack mod 4 = 1 then
          pop_mandatory stack >>= fun (stack, df) ->
          return (stack, Some(df))
        else
          return (stack, None)

      end >>= fun (stack, df_opt) ->
      let (stack, tuples) = pop_iter pop4_opt stack in
      if ImmutStack.is_empty stack then
        let main = tuples |> List.map (fun (d1, d2, d3, d4) -> (d1, (d2, d3), d4)) in
        return ({ cstate with stack }, [ VHCurveTo{ main; df = df_opt } ])
      else
        err Error.CharstringStackRemaining

  | OpHVCurveTo ->
      begin
        if ImmutStack.size stack mod 4 = 1 then
          pop_mandatory stack >>= fun (stack, df) ->
          return (stack, Some(df))
        else
          return (stack, None)
      end >>= fun (stack, df_opt) ->
      let (stack, tuples) = pop_iter pop4_opt stack in
      if ImmutStack.is_empty stack then
        let main = tuples |> List.map (fun (d1, d2, d3, d4) -> (d1, (d2, d3), d4)) in
        return ({ cstate with stack }, [ HVCurveTo{ main; df = df_opt } ])
      else
        err Error.CharstringStackRemaining

  | OpHFlex ->
      pop_mandatory stack >>= fun (stack, dx6) ->
      pop_mandatory stack >>= fun (stack, dx5) ->
      pop_mandatory stack >>= fun (stack, dx4) ->
      pop_mandatory stack >>= fun (stack, dx3) ->
      pop_mandatory stack >>= fun (stack, dy2) ->
      pop_mandatory stack >>= fun (stack, dx2) ->
      pop_mandatory stack >>= fun (stack, dx1) ->
      return ({ cstate with stack }, [ HFlex{ dx1; dv2 = (dx2, dy2); dx3; dx4; dx5; dx6 } ])

  | OpFlex ->
      pop_mandatory stack >>= fun (stack, fd) ->
      pop_mandatory stack >>= fun (stack, dy6) ->
      pop_mandatory stack >>= fun (stack, dx6) ->
      pop_mandatory stack >>= fun (stack, dy5) ->
      pop_mandatory stack >>= fun (stack, dx5) ->
      pop_mandatory stack >>= fun (stack, dy4) ->
      pop_mandatory stack >>= fun (stack, dx4) ->
      pop_mandatory stack >>= fun (stack, dy3) ->
      pop_mandatory stack >>= fun (stack, dx3) ->
      pop_mandatory stack >>= fun (stack, dy2) ->
      pop_mandatory stack >>= fun (stack, dx2) ->
      pop_mandatory stack >>= fun (stack, dy1) ->
      pop_mandatory stack >>= fun (stack, dx1) ->
      return ({ cstate with stack }, [
        Flex{
          dv1 = (dx1, dy1);
          dv2 = (dx2, dy2);
          dv3 = (dx3, dy3);
          dv4 = (dx4, dy4);
          dv5 = (dx5, dy5);
          dv6 = (dx6, dy6);
          fd;
        }
      ])

  | OpHFlex1 ->
      pop_mandatory stack >>= fun (stack, dx6) ->
      pop_mandatory stack >>= fun (stack, dy5) ->
      pop_mandatory stack >>= fun (stack, dx5) ->
      pop_mandatory stack >>= fun (stack, dx4) ->
      pop_mandatory stack >>= fun (stack, dx3) ->
      pop_mandatory stack >>= fun (stack, dy2) ->
      pop_mandatory stack >>= fun (stack, dx2) ->
      pop_mandatory stack >>= fun (stack, dy1) ->
      pop_mandatory stack >>= fun (stack, dx1) ->
      return ({ cstate with stack }, [
        HFlex1{
          dv1 = (dx1, dy1);
          dv2 = (dx2, dy2);
          dx3;
          dx4;
          dv5 = (dx5, dy5);
          dx6;
        }
      ])

  | OpFlex1 ->
      pop_mandatory stack >>= fun (stack, d6) ->
      pop_mandatory stack >>= fun (stack, dy5) ->
      pop_mandatory stack >>= fun (stack, dx5) ->
      pop_mandatory stack >>= fun (stack, dy4) ->
      pop_mandatory stack >>= fun (stack, dx4) ->
      pop_mandatory stack >>= fun (stack, dy3) ->
      pop_mandatory stack >>= fun (stack, dx3) ->
      pop_mandatory stack >>= fun (stack, dy2) ->
      pop_mandatory stack >>= fun (stack, dx2) ->
      pop_mandatory stack >>= fun (stack, dy1) ->
      pop_mandatory stack >>= fun (stack, dx1) ->
      return ({ cstate with stack }, [
        Flex1{
          dv1 = (dx1, dy1);
          dv2 = (dx2, dy2);
          dv3 = (dx3, dy3);
          dv4 = (dx4, dy4);
          dv5 = (dx5, dy5);
          d6;
        }
      ])


and d_subroutine (cconst : charstring_constant) (cstate : charstring_state) (subr : subroutine_index) =
  let open DecodeOperation in
  pop_mandatory cstate.stack >>= fun (stack, i) ->
  let remaining = cstate.lexing.remaining in
  transform_result @@ access_subroutine subr i >>= fun (offset, length, biased_number) ->
  let cstate = { cstate with stack; lexing = { cstate.lexing with remaining = length } } in
  pick offset (d_charstring cconst cstate) >>= fun (cstate, acc) ->
  let cstate =
    { cstate with
      lexing      = { cstate.lexing with remaining };
      used_lsubrs = cstate.used_lsubrs |> IntSet.add biased_number;
    }
  in
  return (cstate, Alist.to_list acc)


and d_charstring (cconst : charstring_constant) (cstate : charstring_state) : (charstring_state * Intermediate.Cff.charstring_operation Alist.t) decoder =
  let open DecodeOperation in
  let rec aux (cstate : charstring_state) acc =
    parse_progress cconst cstate >>= fun (cstate, parsed) ->
    let acc = Alist.append acc parsed in
    let remaining = cstate.lexing.remaining in
    if remaining = 0 then
      return (cstate, acc)
    else if remaining < 0 then
      err @@ CharstringParsingOverrun(remaining)
    else
      aux cstate acc

  in
  aux cstate Alist.empty


let initial_charstring_state (length : int) : charstring_state =
  {
    lexing = {
      remaining = length;
      num_args  = 0;
      num_stems = 0;
    };
    width       = LookingForWidth;
    stack       = ImmutStack.empty;
    used_gsubrs = IntSet.empty;
    used_lsubrs = IntSet.empty;
  }


let charstring (cff : cff_source) (gid : glyph_id) : ((int option * Intermediate.Cff.charstring) option) ok =
  let open ResultMonad in
  let { charstring_info; _ } = cff.cff_specific in
  let { gsubr_index; private_info; offset_CharString_INDEX; _ } = charstring_info in
  fetch_charstring_data cff offset_CharString_INDEX gid >>= function
  | None ->
      return None

  | Some(CharStringData(offset, length)) ->
      select_local_subr_index private_info gid >>= fun lsubr_index ->
      let cconst = { gsubr_index; lsubr_index } in
      let cstate = initial_charstring_state length in
      let dec = d_charstring cconst cstate in
      dec |> DecodeOperation.run cff.cff_common.core offset >>= fun (cstate, acc) ->
      match cstate.width with
      | LookingForWidth    -> err @@ Error.CharstringWithoutWidth
      | WidthDecided(wopt) -> return @@ Some((wopt, Alist.to_list acc))


module LexicalSubroutineIndex : sig
  type t
  val empty : t
  val add : int -> lexical_charstring -> t -> t
  val mem : int -> t -> bool
  val find : int -> t -> lexical_charstring option
  val fold : (int -> lexical_charstring -> 'a -> 'a) -> t -> 'a -> 'a
  val cardinal : t -> int
end = struct

  module Impl = Map.Make(Int)

  type t = lexical_charstring Impl.t

  let empty = Impl.empty

  let add i lcs = Impl.add i lcs

  let mem = Impl.mem

  let find i map =
    map |> Impl.find_opt i

  let fold f map acc =
    Impl.fold f map acc

  let cardinal = Impl.cardinal
end


type lexical_charstring_state = {
  lexical_lexing : charstring_lexing_state;
  lexical_gsubrs : LexicalSubroutineIndex.t;
  lexical_lsubrs : LexicalSubroutineIndex.t;
  last_number    : int option;
}


let rec d_lexical_charstring ~(depth : int) (cconst : charstring_constant) (lcstate : lexical_charstring_state) =
  let open DecodeOperation in
  let open Intermediate.Cff in
  let rec aux (lcstate : lexical_charstring_state) (acc : charstring_token Alist.t) =
    d_charstring_token lcstate.lexical_lexing >>= fun (lstate, cstoken) ->
    let lcstate = { lcstate with lexical_lexing = lstate } in
    let acc = Alist.extend acc cstoken in
    let remaining = lstate.remaining in

    begin
      match cstoken with
      | OpCallSubr ->
          begin
            match lcstate.last_number with
            | None ->
                err Error.NoSubroutineIndexArgument

            | Some(i_biased) ->
                d_lexical_subroutine ~depth ~local:true cconst lcstate i_biased
          end

      | OpCallGSubr ->
          begin
            match lcstate.last_number with
            | None ->
                err Error.NoSubroutineIndexArgument

            | Some(i_biased) ->
                d_lexical_subroutine ~depth ~local:false cconst lcstate i_biased
          end

      | ArgumentInteger(n) ->
          return { lcstate with last_number = Some(n) }

      | _ ->
          return { lcstate with last_number = None }

    end >>= fun lcstate ->
    if remaining = 0 then
      return (lcstate, acc)
    else if remaining < 0 then
      err @@ Error.CharstringParsingOverrun(remaining)
    else
      aux lcstate acc

  in
  aux lcstate Alist.empty


and d_lexical_subroutine ~(depth : int) ~(local : bool) (cconst : charstring_constant) (lcstate : lexical_charstring_state) (i_biased : int) =
  let open DecodeOperation in

  if depth > max_depth_limit then
    err @@ Error.ExceedMaxSubroutineDepth(depth)
  else

    let remaining = lcstate.lexical_lexing.remaining in

    let subrs = if local then cconst.lsubr_index else cconst.gsubr_index in

    transform_result @@ access_subroutine subrs i_biased >>= fun (offset, length, _biased_number) ->
    let lcstate = { lcstate with lexical_lexing = { lcstate.lexical_lexing with remaining = length } } in
    pick offset (d_lexical_charstring ~depth:(depth + 1) cconst lcstate) >>= fun (lcstate, acc) ->
    let lcs = Alist.to_list acc in

    (* Adds the tokenized CharString and resets the remaining byte length. *)
    let lcstate =
      if local then
        { lcstate with
          lexical_lsubrs = lcstate.lexical_lsubrs |> LexicalSubroutineIndex.add i_biased lcs;
          lexical_lexing = { lcstate.lexical_lexing with remaining = remaining };
        }
      else
        { lcstate with
          lexical_gsubrs = lcstate.lexical_gsubrs |> LexicalSubroutineIndex.add i_biased lcs;
          lexical_lexing = { lcstate.lexical_lexing with remaining = remaining };
        }
    in
    return lcstate


let fdindex (cff : cff_source) (gid : glyph_id) : (fdindex option) ok =
  let open ResultMonad in
  let { charstring_info; _ } = cff.cff_specific in
  let { private_info; _ } = charstring_info in
  match private_info with
  | SinglePrivate(_) ->
      return None

  | FontDicts(_, fdselect) ->
      select_fd_index fdselect gid >>= fun fdindex ->
      return @@ Some(fdindex)


let lexical_charstring (cff : cff_source) ~(gsubrs : LexicalSubroutineIndex.t) ~(lsubrs : LexicalSubroutineIndex.t) (gid : glyph_id) : ((LexicalSubroutineIndex.t * LexicalSubroutineIndex.t * lexical_charstring) option) ok =
  let open ResultMonad in
  let { charstring_info; _ } = cff.cff_specific in
  let { gsubr_index; private_info; offset_CharString_INDEX; _ } = charstring_info in
  fetch_charstring_data cff offset_CharString_INDEX gid >>= function
  | None ->
      return None

  | Some(CharStringData(offset, length)) ->
      select_local_subr_index private_info gid >>= fun lsubr_index ->
      let cconst = { gsubr_index; lsubr_index } in
      let lcstate =
        {
          lexical_lexing = {
            remaining = length;
            num_args  = 0;
            num_stems = 0;
          };
          lexical_gsubrs = gsubrs;
          lexical_lsubrs = lsubrs;
          last_number = None;
        }
      in
      let dec = d_lexical_charstring ~depth:0 cconst lcstate in
      dec |> DecodeOperation.run cff.cff_common.core offset >>= fun (lcstate, acc) ->
      return @@ Some((lcstate.lexical_gsubrs, lcstate.lexical_lsubrs, Alist.to_list acc))


let ( +@ ) (x, y) (dx, dy) = (x + dx, y + dy)

let ( +@- ) (x, y) dx = (x + dx, y)

let ( +@| ) (x, y) dy = (x, y + dy)


let line_parity ~starts_horizontally (peacc : cubic_path_element Alist.t) ws curv =
  let (_, peacc, curv) =
    ws |> List.fold_left (fun (is_horizontal, peacc, curv) dt ->
      let curv =
        if is_horizontal then
          curv +@- dt
        else
          curv +@| dt
      in
      (not is_horizontal, Alist.extend peacc (CubicLineTo(curv)), curv)
    ) (starts_horizontally, peacc, curv)
  in
  (curv, peacc)


let curve_parity ~starts_horizontally (peacc : cubic_path_element Alist.t) tuples (dtD, dvE, dsF) dtFopt curv =
  let (is_horizontal, peacc, curv) =
    tuples |> List.fold_left (fun (is_horizontal, peacc, curv) (dtA, dvB, dsC) ->
      if is_horizontal then
        let vA = curv +@- dtA in
        let vB = vA +@ dvB in
        let vC = vB +@| dsC in
        (not is_horizontal, Alist.extend peacc (CubicCurveTo(vA, vB, vC)), vC)
      else
        let vA = curv +@| dtA in
        let vB = vA +@ dvB in
        let vC = vB +@- dsC in
        (not is_horizontal, Alist.extend peacc (CubicCurveTo(vA, vB, vC)), vC)
    ) (starts_horizontally, peacc, curv)
  in
  if is_horizontal then
  (* If `dtD` is x-directed and `dsF` is y-directed *)
    let vD = curv +@- dtD in
    let vE = vD +@ dvE in
    let vF =
      match dtFopt with
      | None      -> vE +@| dsF
      | Some(dtF) -> vE +@ (dtF, dsF)
    in
    (vF, Alist.extend peacc (CubicCurveTo(vD, vE, vF)))
  else
    let vD = curv +@| dtD in
    let vE = vD +@ dvE in
    let vF =
      match dtFopt with
      | None      -> vE +@- dsF
      | Some(dtF) -> vE +@ (dsF, dtF)
    in
    (vF, Alist.extend peacc (CubicCurveTo(vD, vE, vF)))


let flex_path ~current:curv pt1 pt2 pt3 pt4 pt5 pt6 =
  let abspt1 = curv +@ pt1 in
  let abspt2 = abspt1 +@ pt2 in
  let abspt3 = abspt2 +@ pt3 in
  let abspt4 = abspt3 +@ pt4 in
  let abspt5 = abspt4 +@ pt5 in
  let abspt6 = abspt5 +@ pt6 in
  let curv = abspt6 in
  (curv, [CubicCurveTo(abspt1, abspt2, abspt3); CubicCurveTo(abspt4, abspt5, abspt6)])


type path_reading_state_middle = {
  start : point;
  elems : cubic_path_element Alist.t;
  paths : cubic_path Alist.t;
}

type path_reading_state =
  | Initial
  | Middle of path_reading_state_middle


let start_new_path (state : path_reading_state) (curv : point) : path_reading_state_middle =
  match state with
  | Initial ->
      { start = curv; elems = Alist.empty; paths = Alist.empty }

  | Middle(middle) ->
      let path = (middle.start, Alist.to_list middle.elems) in
      { start = curv; elems = Alist.empty; paths = Alist.extend middle.paths path }


let assert_middle =
  let open ResultMonad in
  function
  | Initial        -> err Error.NotAMiddleOfPathInCharstring
  | Middle(middle) -> return middle


let chop_last_of_list xs =
  let open ResultMonad in
  match List.rev xs with
  | []               -> err Error.EmptyCurveInCharstring
  | last :: main_rev -> return (List.rev main_rev, last)


let path_of_charstring (ops : Intermediate.Cff.charstring) : (cubic_path list) ok =
  let open ResultMonad in
  let open Intermediate.Cff in
  ops |> List.fold_left (fun prevres op ->
    prevres >>= fun (curv, state) ->
    match op with
    | HintMask(_)
    | CntrMask(_)
    | HStem(_)
    | VStem(_)
    | HStemHM(_)
    | VStemHM(_) ->
        return (curv, state)

    | VMoveTo{ dy1 } ->
        let curv = curv +@| dy1 in
        let middle = start_new_path state curv in
        return (curv, Middle(middle))

    | HMoveTo{ dx1 } ->
        let curv = curv +@- dx1 in
        let middle = start_new_path state curv in
        return (curv, Middle(middle))

    | RMoveTo{ dv1 } ->
        let curv = curv +@ dv1 in
        let middle = start_new_path state curv in
        return (curv, Middle(middle))

    | RLineTo(cspts) ->
        assert_middle state >>= fun middle ->
        let (curv, peacc) =
          cspts |> List.fold_left (fun (curv, peacc) dv ->
            (curv +@ dv, Alist.extend peacc (CubicLineTo(curv +@ dv)))
          ) (curv, middle.elems)
        in
        return (curv, Middle{ middle with elems = peacc })

    | HLineTo(ws) ->
        assert_middle state >>= fun middle ->
        let (curv, peacc) = line_parity ~starts_horizontally:true middle.elems ws curv in
        return (curv, Middle{ middle with elems = peacc })

    | VLineTo(ws) ->
        assert_middle state >>= fun middle ->
        let (curv, peacc) = line_parity ~starts_horizontally:false middle.elems ws curv in
        return (curv, Middle{ middle with elems = peacc })

    | RRCurveTo(tricspts) ->
        assert_middle state >>= fun middle ->
        let (curv, peacc) =
          tricspts |> List.fold_left (fun (curv, peacc) (dvA, dvB, dvC) ->
            let vA = curv +@ dvA in
            let vB = vA +@ dvB in
            let vC = vB +@ dvC in
            (vC, Alist.extend peacc (CubicCurveTo(vA, vB, vC)))
          ) (curv, middle.elems)
        in
        return (curv, Middle{ middle with elems = peacc })

    | VVCurveTo{ dx1 = _; rest = [] } ->
        err @@ Error.EmptyCurveInCharstring

    | VVCurveTo{ dx1 = dx1_opt; rest = (dy1, dv2, dy3) :: vvs } ->
        assert_middle state >>= fun middle ->
        let v1 =
          match dx1_opt with
          | None      -> curv +@| dy1
          | Some(dx1) -> curv +@ (dx1, dy1)
        in
        let v2 = v1 +@ dv2 in
        let v3 = v2 +@| dy3 in
        let (curv, peacc) =
          vvs |> List.fold_left (fun (curv, peacc) (dyA, dvB, dyC) ->
            let vA = curv +@| dyA in
            let vB = vA +@ dvB in
            let vC = vB +@| dyC in
            (vC, Alist.extend peacc (CubicCurveTo(vA, vB, vC)))
          ) (v3, Alist.extend middle.elems (CubicCurveTo(v1, v2, v3)))
        in
        return (curv, Middle{ middle with elems = peacc })

    | HHCurveTo{ dy1 = _; rest = [] } ->
        err @@ Error.EmptyCurveInCharstring

    | HHCurveTo{ dy1 = dy1_opt; rest = (dx1, dv2, dx3) :: hhs } ->
        assert_middle state >>= fun middle ->
        let v1 =
          match dy1_opt with
          | None      -> curv +@- dx1
          | Some(dy1) -> curv +@ (dx1, dy1)
        in
        let v2 = v1 +@ dv2 in
        let v3 = v2 +@- dx3 in
        let (curv, peacc) =
          hhs |> List.fold_left (fun (curv, peacc) (dxA, dvB, dxC) ->
            let vA = curv +@- dxA in
            let vB = vA +@ dvB in
            let vC = vB +@- dxC in
            (vC, Alist.extend peacc (CubicCurveTo(vA, vB, vC)))
          ) (v3, Alist.extend middle.elems (CubicCurveTo(v1, v2, v3)))
        in
        return (curv, Middle{ middle with elems = peacc })

    | HVCurveTo{ main = hvs; df = df_opt } ->
        assert_middle state >>= fun middle ->
        chop_last_of_list hvs >>= fun (hvsmain, last) ->
        let (curv, peacc) = curve_parity ~starts_horizontally:true middle.elems hvsmain last df_opt curv in
        return (curv, Middle{ middle with elems = peacc })

    | VHCurveTo{ main = vhs; df = df_opt } ->
        assert_middle state >>= fun middle ->
        chop_last_of_list vhs >>= fun (vhsmain, last) ->
        let (curv, peacc) = curve_parity ~starts_horizontally:false middle.elems vhsmain last df_opt curv in
        return (curv, Middle{ middle with elems = peacc })

    | Flex{ dv1; dv2; dv3; dv4; dv5; dv6; fd = _ } ->
        assert_middle state >>= fun middle ->
        let (curv, pes_flex) = flex_path ~current:curv dv1 dv2 dv3 dv4 dv5 dv6 in
        let peacc = Alist.append middle.elems pes_flex in
        return (curv, Middle{ middle with elems = peacc })

    | HFlex{ dx1; dv2; dx3; dx4; dx5; dx6 } ->
        let (_dx2, dy2) = dv2 in
        assert_middle state >>= fun middle ->
        let (curv, pes_flex) = flex_path ~current:curv (dx1, 0) dv2 (dx3, 0) (dx4, 0) (dx5, -dy2) (dx6, 0) in
        let peacc = Alist.append middle.elems pes_flex in
        return (curv, Middle{ middle with elems = peacc })

    | HFlex1{ dv1; dv2; dx3; dx4; dv5; dx6 } ->
        let (_dx1, dy1) = dv1 in
        let (_dx2, dy2) = dv2 in
        let (_dx5, dy5) = dv5 in
        assert_middle state >>= fun middle ->
        let dy6 = -(dy1 + dy2 + dy5) in
        let (curv, pes_flex) = flex_path ~current:curv dv1 dv2 (dx3, 0) (dx4, 0) dv5 (dx6, dy6) in
        let peacc = Alist.append middle.elems pes_flex in
        return (curv, Middle{ middle with elems = peacc })

    | Flex1{ dv1 = pt1; dv2 = pt2; dv3 = pt3; dv4 = pt4; dv5 = pt5; d6 } ->
        assert_middle state >>= fun middle ->
        let (dxsum, dysum) = pt1 +@ pt2 +@ pt3 +@ pt4 +@ pt5 in
        let (xstart, ystart) = curv in
        let abspt1 = curv +@ pt1 in
        let abspt2 = abspt1 +@ pt2 in
        let abspt3 = abspt2 +@ pt3 in
        let abspt4 = abspt3 +@ pt4 in
        let abspt5 = abspt4 +@ pt5 in
        let (absx5, absy5) = abspt5 in
        let abspt6 =
          if abs dxsum > abs dysum then
            (absx5 + d6, ystart)
          else
            (xstart, absy5 + d6)
        in
        let curv = abspt6 in
        let pes_flex = [CubicCurveTo(abspt1, abspt2, abspt3); CubicCurveTo(abspt4, abspt5, abspt6)] in
        let peacc = Alist.append middle.elems pes_flex in
        return (curv, Middle{ middle with elems = peacc })

    ) (return ((0, 0), Initial)) >>= function
    | (_, Initial) ->
        return []

    | (_, Middle(middle)) ->
        let path = (middle.start, Alist.to_list middle.elems) in
        return @@ Alist.to_list (Alist.extend middle.paths path)


(* TODO: remove this; temporary *)
let get_bias (cff : cff_source) (fdindex_opt : fdindex option) : int =
  let private_info = cff.cff_specific.charstring_info.private_info in
  match (private_info, fdindex_opt) with
  | (SinglePrivate{ local_subr_index; _ }, None) ->
      convert_subroutine_number local_subr_index 0

  | (FontDicts(fdarray, _fdselect), Some(fdindex)) ->
      let single_private = fdarray.(fdindex) in
      convert_subroutine_number single_private.local_subr_index 0

  | (SinglePrivate(_), Some(_)) ->
      -100000

  | (FontDicts(_, _), None) ->
      -200000
