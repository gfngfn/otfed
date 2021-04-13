
open Basic
open Value
open DecodeBasic
open DecodeOperation.Open


module Maxp = DecodeCffMaxp


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


let fetch_number_of_glyphs (core : common_source_core) ~(offset_CharString_INDEX : offset) : int ok =
  let open DecodeOperation in
  d_uint16 |> run core offset_CharString_INDEX


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


let fetch_single_private (core : common_source_core) ~(offset_CFF : offset) (dict : dict) : single_private ok =
  (* Private DICT *)
  let open ResultMonad in
  get_integer_pair_opt dict (ShortKey(18)) >>= function
  | None ->
      err Error.NoPrivateDict

  | Some(size_private, reloffset_private) ->
      let offset_private = offset_CFF + reloffset_private in
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


let fetch_cff_specific (core : common_source_core) (table_directory : table_directory) : cff_specific ok =
  let open ResultMonad in
  DecodeOperation.seek_required_table table_directory Tag.table_cff >>= fun (offset_CFF, _length) ->
  fetch_cff_first ~offset_CFF core >>= fun cff_first ->
  let font_name    = cff_first.cff_first_name in
  let top_dict     = cff_first.cff_first_top_dict in
  let string_index = cff_first.cff_first_string_index in
  let gsubr_index  = cff_first.cff_first_gsubr_index in
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
    fetch_number_of_glyphs core ~offset_CharString_INDEX >>= fun number_of_glyphs ->
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
        fetch_fdarray core ~offset_CFF ~offset_FDArray >>= fun fdarray ->
        fetch_fdselect core ~number_of_glyphs ~offset_FDSelect >>= fun fdselect ->
        return (Some{
          registry; ordering; supplement;
          cid_font_version;
          cid_font_revision;
          cid_font_type;
          cid_count;
        }, FontDicts(fdarray, fdselect))
    else
    (* If the font is not a CIDFont *)
      fetch_single_private core ~offset_CFF top_dict >>= fun singlepriv ->
      return (None, SinglePrivate(singlepriv))
  end >>= fun (cid_info, private_info) ->
  let cff_top_dict =
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
  let charstring_info =
    {
      gsubr_index;
      private_info;
      offset_CharString_INDEX;
    }
  in
  return { cff_top_dict; charstring_info }


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


let select_local_subr_index (private_info : private_info) (gid : glyph_id) =
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


let d_stem_argument (num_stems : int) : (int * stem_argument) decoder =
  let open DecodeOperation in
  let arglen =
    if num_stems mod 8 = 0 then
      num_stems / 8
    else
      num_stems / 8 + 1
  in
  d_bytes arglen >>= fun arg ->
  return (arglen, arg)


let d_charstring_element (lstate : charstring_lexing_state) : (charstring_lexing_state * charstring_element) decoder =
  let open DecodeOperation in
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
            err @@ Error.InvalidCharstring
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

  | _ ->
      assert false
      (* `uint8`-typed value must be in [0 .. 255] *)


let pop_mandatory (stack : stack) : (stack * int) decoder =
  let open DecodeOperation in
  match ImmutStack.pop stack with
  | None       -> err Error.InvalidCharstring
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
  try
    let biased_number = convert_subroutine_number subr_index i in
    let CharStringData(offset, length) = subr_index.(biased_number) in
    return (offset, length, biased_number)
  with
  | Invalid_argument(_) ->
      err Error.InvalidCharstring


let rec parse_progress (cconst : charstring_constant) (cstate : charstring_state) =
  let open DecodeOperation in
  d_charstring_element cstate.lexing >>= fun (lstate, cselem) ->
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
            err Error.InvalidCharstring

        | (y, dy) :: cspts ->
            let (stack, width) = pop_opt_for_width cstate.width stack in
            return ({ cstate with width; stack }, [HStem(y, dy, cspts)])
      end

  | OpVStem ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      begin
        match pairs with
        | [] ->
            err Error.InvalidCharstring

        | (x, dx) :: cspts ->
            let (stack, width) = pop_opt_for_width cstate.width stack in
            return ({ cstate with width; stack }, [VStem(x, dx, cspts)])
      end

  | OpVMoveTo ->
      pop_mandatory stack >>= fun (stack, arg) ->
      let (stack, width) = pop_opt_for_width cstate.width stack in
      return ({ cstate with width; stack }, [VMoveTo(arg)])

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
        err Error.InvalidCharstring

  | OpHStemHM ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      begin
        match pairs with
        | [] ->
            err Error.InvalidCharstring

        | (y, dy) :: cspts ->
            let (stack, width) = pop_opt_for_width cstate.width stack in
            return ({ cstate with width; stack }, [HStemHM(y, dy, cspts)])
      end

  | OpHintMask(arg) ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      let (stack, width) = pop_opt_for_width cstate.width stack in
      let cstate = { cstate with width; stack } in
      begin
        match pairs with
        | []               -> return (cstate, [HintMask(arg)])
        | (x, dx) :: cspts -> return (cstate, [VStemHM(x, dx, cspts); HintMask(arg)])
      end

  | OpCntrMask(arg) ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      let (stack, width) = pop_opt_for_width cstate.width stack in
      let cstate = { cstate with width; stack } in
      begin
        match pairs with
        | []               -> return (cstate, [CntrMask(arg)])
        | (x, dx) :: cspts -> return (cstate, [VStemHM(x, dx, cspts); CntrMask(arg)])
      end

  | OpRMoveTo ->
      pop_mandatory stack >>= fun (stack, dy1) ->
      pop_mandatory stack >>= fun (stack, dx1) ->
      let (stack, width) = pop_opt_for_width cstate.width stack in
      return ({ cstate with width; stack }, [RMoveTo((dx1, dy1))])

  | OpHMoveTo ->
      pop_mandatory stack >>= fun (stack, arg) ->
      let (stack, width) = pop_opt_for_width cstate.width stack in
      return ({ cstate with width; stack }, [HMoveTo(arg)])

  | OpVStemHM ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      begin
        match pairs with
        | [] ->
            err Error.InvalidCharstring

        | (x, dx) :: cspts ->
            let (stack, width) = pop_opt_for_width cstate.width stack in
            return ({ cstate with width; stack }, [VStemHM(x, dx, cspts)])
      end

  | OpRCurveLine ->
      pop_mandatory stack >>= fun (stack, dyd) ->
      pop_mandatory stack >>= fun (stack, dxd) ->
      let (stack, tuples) = pop_iter pop6_opt stack in
      let beziers = tuples |> List.map make_bezier in
      return ({ cstate with stack }, [RRCurveTo(beziers); RLineTo([(dxd, dyd)])])

  | OpRLineCurve ->
      pop_mandatory stack >>= fun (stack, dyd) ->
      pop_mandatory stack >>= fun (stack, dxd) ->
      pop_mandatory stack >>= fun (stack, dyc) ->
      pop_mandatory stack >>= fun (stack, dxc) ->
      pop_mandatory stack >>= fun (stack, dyb) ->
      pop_mandatory stack >>= fun (stack, dxb) ->
      let (stack, pairs) = pop_iter pop2_opt stack in
      return ({ cstate with stack }, [RLineTo(pairs); RRCurveTo([((dxb, dyb), (dxc, dyc), (dxd, dyd))])])

  | OpVVCurveTo ->
      let (stack, tuples) = pop_iter pop4_opt stack in
      let rets = tuples |> List.map (fun (dya, dxb, dyb, dyc) -> (dya, (dxb, dyb), dyc)) in
      let (stack, dx1opt) = pop_opt stack in
      return ({ cstate with stack }, [VVCurveTo(dx1opt, rets)])

  | OpHHCurveTo ->
      let (stack, tuples) = pop_iter pop4_opt stack in
      let rets = tuples |> List.map (fun (dxa, dxb, dyb, dxc) -> (dxa, (dxb, dyb), dxc)) in
      let (stack, dy1opt) = pop_opt stack in
      return ({ cstate with stack }, [HHCurveTo(dy1opt, rets)])

  | OpCallGSubr ->
      d_subroutine cconst cstate cconst.gsubr_index

  | OpVHCurveTo ->
      begin
        if ImmutStack.size stack mod 4 = 1 then
          pop_mandatory stack >>= fun (stack, df) ->
          return (stack, Some(df))
        else
          return (stack, None)

      end >>= fun (stack, dfopt) ->
      let (stack, tuples) = pop_iter pop4_opt stack in
      if ImmutStack.is_empty stack then
        let rets = tuples |> List.map (fun (d1, d2, d3, d4) -> (d1, (d2, d3), d4)) in
        return ({ cstate with stack }, [VHCurveTo(rets, dfopt)])
      else
        err Error.InvalidCharstring

  | OpHVCurveTo ->
      begin
        if ImmutStack.size stack mod 4 = 1 then
          pop_mandatory stack >>= fun (stack, df) ->
          return (stack, Some(df))
        else
          return (stack, None)
      end >>= fun (stack, dfopt) ->
      let (stack, tuples) = pop_iter pop4_opt stack in
      if ImmutStack.is_empty stack then
        let rets = tuples |> List.map (fun (d1, d2, d3, d4) -> (d1, (d2, d3), d4)) in
        return ({ cstate with stack }, [HVCurveTo(rets, dfopt)])
      else
        err Error.InvalidCharstring

  | OpHFlex ->
      pop_mandatory stack >>= fun (stack, dx6) ->
      pop_mandatory stack >>= fun (stack, dx5) ->
      pop_mandatory stack >>= fun (stack, dx4) ->
      pop_mandatory stack >>= fun (stack, dx3) ->
      pop_mandatory stack >>= fun (stack, dy2) ->
      pop_mandatory stack >>= fun (stack, dx2) ->
      pop_mandatory stack >>= fun (stack, dx1) ->
      return ({ cstate with stack }, [HFlex(dx1, (dx2, dy2), dx3, dx4, dx5, dx6)])

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
      let parsed = [Flex((dx1, dy1), (dx2, dy2), (dx3, dy3), (dx4, dy4), (dx5, dy5), (dx6, dy6), fd)] in
      return ({ cstate with stack }, parsed)

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
      return ({ cstate with stack }, [HFlex1((dx1, dy1), (dx2, dy2), dx3, dx4, (dx5, dy5), dx6)])

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
      return ({ cstate with stack }, [Flex1((dx1, dy1), (dx2, dy2), (dx3, dy3), (dx4, dy4), (dx5, dy5), d6)])


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


and d_charstring (cconst : charstring_constant) (cstate : charstring_state) : (charstring_state * charstring_operation Alist.t) decoder =
  let open DecodeOperation in
  let rec aux (cstate : charstring_state) acc =
    parse_progress cconst cstate >>= fun (cstate, parsed) ->
    let acc = Alist.append acc parsed in
    let remaining = cstate.lexing.remaining in
    if remaining = 0 then
      return (cstate, acc)
    else if remaining < 0 then
      err @@ InvalidCharstring
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


let charstring (cff : cff_source) (gid : glyph_id) : ((int option * charstring) option) ok =
  let open ResultMonad in
  let { charstring_info; _ } = cff.cff_specific in
  let { gsubr_index; private_info; offset_CharString_INDEX } = charstring_info in
  fetch_charstring_data cff offset_CharString_INDEX gid >>= function
  | None ->
      return None

  | Some(CharStringData(offset, length)) ->
      select_local_subr_index private_info gid >>= fun lsubr_index ->
      let cconst =
        {
          gsubr_index;
          lsubr_index;
        }
      in
      let cstate = initial_charstring_state length in
      let dec = d_charstring cconst cstate in
      dec |> DecodeOperation.run cff.cff_common.core offset >>= fun (cstate, acc) ->
      match cstate.width with
      | LookingForWidth    -> err @@ Error.CharstringWithoutWidth
      | WidthDecided(wopt) -> return @@ Some((wopt, Alist.to_list acc))


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
  | Initial        -> err Error.InvalidCharstring
  | Middle(middle) -> return middle


let chop_last_of_list xs =
  let open ResultMonad in
  match List.rev xs with
  | []               -> err Error.InvalidCharstring
  | last :: main_rev -> return (List.rev main_rev, last)


let path_of_charstring (ops : charstring) : (cubic_path list) ok =
  let open ResultMonad in
  ops |> List.fold_left (fun prevres op ->
    prevres >>= fun (curv, state) ->
    match op with
    | HintMask(_)
    | CntrMask(_)
    | HStem(_, _, _)
    | VStem(_, _, _)
    | HStemHM(_, _, _)
    | VStemHM(_, _, _) ->
        return (curv, state)

    | VMoveTo(dy) ->
        let curv = curv +@| dy in
        let middle = start_new_path state curv in
        return (curv, Middle(middle))

    | HMoveTo(dx) ->
        let curv = curv +@- dx in
        let middle = start_new_path state curv in
        return (curv, Middle(middle))

    | RMoveTo(dv) ->
        let curv = curv +@ dv in
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

    | VVCurveTo(_, []) ->
        err Error.InvalidCharstring

    | VVCurveTo(dx1opt, (dy1, dv2, dy3) :: vvs) ->
        assert_middle state >>= fun middle ->
        let v1 =
          match dx1opt with
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

    | HHCurveTo(_, []) ->
        err Error.InvalidCharstring

    | HHCurveTo(dy1opt, (dx1, dv2, dx3) :: hhs) ->
        assert_middle state >>= fun middle ->
        let v1 =
          match dy1opt with
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

    | HVCurveTo(hvs, dfopt) ->
        assert_middle state >>= fun middle ->
        chop_last_of_list hvs >>= fun (hvsmain, last) ->
        let (curv, peacc) = curve_parity ~starts_horizontally:true middle.elems hvsmain last dfopt curv in
        return (curv, Middle{ middle with elems = peacc })

    | VHCurveTo(vhs, dfopt) ->
        assert_middle state >>= fun middle ->
        chop_last_of_list vhs >>= fun (vhsmain, last) ->
        let (curv, peacc) = curve_parity ~starts_horizontally:false middle.elems vhsmain last dfopt curv in
        return (curv, Middle{ middle with elems = peacc })

    | Flex(pt1, pt2, pt3, pt4, pt5, pt6, _) ->
        assert_middle state >>= fun middle ->
        let (curv, pes_flex) = flex_path ~current:curv pt1 pt2 pt3 pt4 pt5 pt6 in
        let peacc = Alist.append middle.elems pes_flex in
        return (curv, Middle{ middle with elems = peacc })

    | HFlex(dx1, (dx2, dy2), dx3, dx4, dx5, dx6) ->
        assert_middle state >>= fun middle ->
        let (curv, pes_flex) = flex_path ~current:curv (dx1, 0) (dx2, dy2) (dx3, 0) (dx4, 0) (dx5, -dy2) (dx6, 0) in
        let peacc = Alist.append middle.elems pes_flex in
        return (curv, Middle{ middle with elems = peacc })

    | HFlex1((dx1, dy1), (dx2, dy2), dx3, dx4, (dx5, dy5), dx6) ->
        assert_middle state >>= fun middle ->
        let dy6 = - (dy1 + dy2 + dy5) in
        let (curv, pes_flex) = flex_path ~current:curv (dx1, dy1) (dx2, dy2) (dx3, 0) (dx4, 0) (dx5, dy5) (dx6, dy6) in
        let peacc = Alist.append middle.elems pes_flex in
        return (curv, Middle{ middle with elems = peacc })

    | Flex1(pt1, pt2, pt3, pt4, pt5, d6) ->
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
