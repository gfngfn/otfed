
open Basic
open Value
open DecodeBasic


include DecodeOperationCore


let d_uint32_int : int decoder =
  d_uint32 >>= fun n ->
  return @@ WideInt.to_int n


let d_code_point : Uchar.t decoder =
  d_uint32_int >>= fun n ->
  if Uchar.is_valid n then
    return @@ Uchar.of_int n
  else
    err @@ Error.InvalidCodePoint(n)


let d_f2dot14 : float decoder =
  d_int16 >>= fun n ->
  return ((float n) /. 16384.0)


let pick (offset : offset) (dec : 'a decoder) : 'a decoder =
  current >>= fun pos_before ->
  seek offset >>= fun () ->
  dec >>= fun v ->
  seek pos_before >>= fun () ->
  return v


let pick_each (offsets : offset list) (dec : 'a decoder) : ('a list) decoder =
  offsets |> mapM (fun offset -> pick offset dec)


let d_offset (offset_origin : offset) : offset decoder =
  d_uint16 >>= fun reloffset ->
  return (offset_origin + reloffset)


let d_offset_opt (offset_origin : int) : (offset option) decoder =
  d_uint16 >>= fun reloffset ->
  if reloffset = 0 then
    return None
  else
    return @@ Some(offset_origin + reloffset)


let d_fetch (offset_origin : offset) (dec : 'a decoder) : 'a decoder =
  d_offset offset_origin >>= fun offset ->
  pick offset dec


let d_fetch_long (origin : offset) (dec : 'a decoder) : (offset * 'a) decoder =
  current >>= fun pos_before ->
  d_uint32_int >>= fun reloffset ->
  let offset = origin + reloffset in
  seek offset >>= fun () ->
  dec >>= fun v ->
  seek (pos_before + 4) >>= fun () ->
  return (offset, v)


let d_repeat : 'a. int -> 'a decoder -> ('a list) decoder =
fun count dec ->
  let rec aux acc i =
    if i <= 0 then
      return @@ Alist.to_list acc
    else
      dec >>= fun v ->
      aux (Alist.extend acc v) (i - 1)
  in
  aux Alist.empty count


let d_list dec =
  d_uint16 >>= fun count ->
  d_repeat count dec


let d_list_filtered : 'a decoder -> (int -> bool) -> ('a list) decoder =
fun dec predicate ->
  let rec aux acc imax i =
    if i >= imax then
      return @@ Alist.to_list acc
    else
      dec >>= fun data ->
      if predicate i then
        aux (Alist.extend acc data) imax (i + 1)
      else
        aux acc imax (i + 1)
  in
  d_uint16 >>= fun count ->
  aux Alist.empty count 0


let d_tag : Value.Tag.t decoder =
  d_uint32 >>= fun n ->
  return @@ Value.Tag.of_wide_int n


let d_loc_format : loc_format decoder =
  d_uint16 >>= function
  | 0 -> return ShortLocFormat
  | 1 -> return LongLocFormat
  | i -> err @@ InvalidLocFormat(i)


type format_version_result =
  | InitTtf
  | InitCff
  | InitCollection


let d_format_version : format_version_result decoder =
  d_tag >>= fun tag ->
  let open Value.Tag in
  if equal tag format_version_OTTO then
    return InitCff
  else if equal tag format_version_true || equal tag format_version_1_0 then
    return InitTtf
  else if equal tag format_version_ttcf then
    return InitCollection
  else
    err @@ UnknownFormatVersion(tag)


let d_long_offset_list : (offset list) decoder =
  d_uint32_int >>= fun count ->
  d_repeat count d_uint32_int


let d_ttc_header_offset_list : (offset list) decoder =
  d_uint32 >>= fun ttc_version ->
  if WideInt.equal ttc_version (!%% 0x00010000L) || WideInt.equal ttc_version (!%% 0x00020000L) then
    d_long_offset_list
  else
    err @@ UnknownTtcVersion(ttc_version)


type table_record = Value.Tag.t * offset * int


let d_table_record : table_record decoder =
  d_tag >>= fun tag ->
  d_skip 4 >>= fun () ->
  d_uint32_int >>= fun offset ->
  d_uint32_int >>= fun length ->
  return (tag, offset, length)


let d_structure : table_directory decoder =
  d_uint16 >>= fun numTables ->
  d_skip (3 * 2) >>= fun () ->
  d_repeat numTables d_table_record >>= fun records ->
  let map =
    records |> List.fold_left (fun map (tag, offset, length) ->
      map |> TableDirectory.add tag (offset, length)
    ) TableDirectory.empty
  in
  return map


let d_range_record : (glyph_id list) decoder =
  let rec range acc i j =
    if i > j then Alist.to_list acc else
      range (Alist.extend acc i) (i + 1) j
  in
  d_uint16 >>= fun start_gid ->
  d_uint16 >>= fun end_gid ->
  d_skip 2 >>= fun () -> (* Skips `startCoverageIndex` *)
  return (range Alist.empty start_gid end_gid)


let d_coverage : (glyph_id list) decoder =
  (* The position is supposed to be set to the beginning of a Coverage table [page 139]. *)
  d_uint16 >>= fun coverageFormat ->
  match coverageFormat with
  | 1 -> d_list d_uint16
  | 2 -> d_list d_range_record >>= fun ranges -> return (List.concat ranges)
  | _ -> err @@ Error.UnknownCoverageFormat(coverageFormat)


let combine_coverage : 'a. glyph_id list -> 'a list -> ((glyph_id * 'a) list) decoder =
fun coverage vs ->
  try return (List.combine coverage vs) with
  | Invalid_argument(_) -> err @@ Error.InvalidCoverageLength


let d_offsize : offsize decoder =
  d_uint8 >>= function
  | 1 -> return OffSize1
  | 2 -> return OffSize2
  | 3 -> return OffSize3
  | 4 -> return OffSize4
  | n -> err @@ Error.InvalidOffsize(n)


let d_cff_offset (offsize : offsize) : wint decoder =
  match offsize with
  | OffSize1 -> d_uint8  >>= fun i -> return (!% i)
  | OffSize2 -> d_uint16 >>= fun i -> return (!% i)
  | OffSize3 -> d_uint24 >>= fun i -> return (!% i)
  | OffSize4 -> d_uint32


let d_twoscompl2 =
  d_uint8 >>= fun b1 ->
  d_uint8 >>= fun b2 ->
  let iraw = (b1 lsl 8) lor b2 in
  let ret =
    if iraw >= (1 lsl 15) then
      iraw - (1 lsl 16)
    else
      iraw
  in
  return ret


let d_twoscompl4 =
  d_uint8 >>= fun b1 ->
  d_uint8 >>= fun b2 ->
  d_uint8 >>= fun b3 ->
  d_uint8 >>= fun b4 ->
  let iraw = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in
  let ret =
    if iraw >= (1 lsl 31) then
      iraw - (1 lsl 32)
    else
      iraw
  in
  return ret


let d_cff_real =
  let to_float ss =
    float_of_string (String.concat "" ss)
  in
  let nibble = function
    | d when d |> is_in_range ~lower:0 ~upper:9 -> return (string_of_int d)
    | 10                                        -> return "."
    | 11                                        -> return "e"
    | 12                                        -> return "e-"
    | 13                                        -> err NotACffDictElement
    | 14                                        -> return "-"
    | 15                                        -> return ""
    | _                                         -> err NotACffDictElement
  in
  let rec aux step acc =
    d_uint8 >>= fun raw ->
    let q1 = raw / 16 in
    let q2 = raw mod 16 in
    if q1 = 15 then
      if q2 = 15 then
        return (step + 1, to_float (Alist.to_list acc))
      else
        err NotACffDictElement
    else
      if q2 = 15 then
        nibble q1 >>= fun nb1 ->
        return (step + 1, to_float (Alist.to_list (Alist.extend acc nb1)))
      else
        nibble q2 >>= fun nb2 ->
        nibble q1 >>= fun nb1 ->
        aux (step + 1) (Alist.extend (Alist.extend acc nb1) nb2)
  in
  aux 0 Alist.empty


let d_cff_length_list offsize count =
  let rec aux offset_prev acc i =
    if i >= count then
      return @@ Alist.to_list acc
    else
      d_cff_offset offsize >>= fun offset ->
      let len = WideInt.to_int (offset -% offset_prev) in
      aux offset (Alist.extend acc len) (i + 1)
  in
  d_cff_offset offsize >>= fun offset1 ->
  if offset1 <> !% 1 then
    err @@ InvalidFirstOffsetInIndex(offset1)
  else
    aux (!% 1) Alist.empty 0


let d_index : 'a. (int -> 'a decoder) -> ('a list) decoder =
fun ldec ->
  let rec aux acc = function
    | [] ->
        return @@ Alist.to_list acc

    | length :: lengths ->
        ldec length >>= fun v ->
        aux (Alist.extend acc v) lengths
  in
  d_uint16 >>= fun count ->
  if count = 0 then
    return []
  else
    d_offsize >>= fun offsize ->
    d_cff_length_list offsize count >>= fun lengths ->
    aux Alist.empty lengths


let d_cff_offset_singleton offsize ldec =
  d_cff_offset offsize >>= fun offset1 ->
  if offset1 <> !% 1 then
    err @@ InvalidFirstOffsetInIndex(offset1)
  else
    d_cff_offset offsize >>= fun offset2 ->
    ldec (WideInt.to_int (offset2 -% offset1))


let d_index_singleton : 'a. (int -> 'a decoder) -> 'a decoder =
fun ldec ->
  d_uint16 >>= fun count ->
  if count <> 1 then
    err @@ NotASingletonIndex
  else
    d_offsize >>= fun offsize ->
    d_cff_offset_singleton offsize ldec


let d_index_access : 'a. (int -> 'a decoder) -> int -> ('a option) decoder =
fun ldec i ->
  d_uint16 >>= fun count ->
  if i < 0 || count <= i then
    return None
  else
    d_offsize >>= fun offsize ->
    let offsize_int =
      match offsize with
      | OffSize1 -> 1
      | OffSize2 -> 2
      | OffSize3 -> 3
      | OffSize4 -> 4
    in
    current >>= fun pos ->
    let offset_origin = pos + (count + 1) * offsize_int - 1 in
    d_skip (i * offsize_int) >>= fun () ->
    d_cff_offset offsize >>= fun reloffset_access ->
    d_cff_offset offsize >>= fun reloffset_next ->
    let offset_access = offset_origin + (WideInt.to_int reloffset_access) in
    let data_length = WideInt.to_int (reloffset_next -% reloffset_access) in
    seek offset_access >>= fun () ->
    ldec data_length >>= fun data ->
    return (Some(data))


let d_dict_element : (int * dict_element) decoder =
  d_uint8 >>= function
  | k0 when k0 |> is_in_range ~lower:0 ~upper:11 ->
      return (1, Key(ShortKey(k0)))

  | 12 ->
      d_uint8 >>= fun k1 ->
      return (2, Key(LongKey(k1)))

  | k0 when k0 |> is_in_range ~lower:13 ~upper:21 ->
      return (1, Key(ShortKey(k0)))

  | 28 ->
      d_twoscompl2 >>= fun ret ->
      return (3, Value(Integer(ret)))

  | 29 ->
      d_twoscompl4 >>= fun ret ->
      return (5, Value(Integer(ret)))

  | 30 ->
      d_cff_real >>= fun (step, real) ->
      return (1 + step, Value(Real(real)))

  | b0 when b0 |> is_in_range ~lower:32 ~upper:246 ->
      return (1, Value(Integer(b0 - 139)))

  | b0 when b0 |> is_in_range ~lower:247 ~upper:250 ->
      d_uint8 >>= fun b1 ->
      return (2, Value(Integer((b0 - 247) * 256 + b1 + 108)))

  | b0 when b0 |> is_in_range ~lower:251 ~upper:254 ->
      d_uint8 >>= fun b1 ->
      return (2, Value(Integer(-(b0 - 251) * 256 - b1 - 108)))

  | _ ->
      err @@ Error.NotACffDictElement


(* `d_dict_keyval d` returns `(steps, vs, key)` where:
   - `steps`: how many bytes the decoder ran to read the single key-value pair,
   - `vs`   : the list of operands, and
   - `key`  : the operator. *)
let d_dict_keyval : (int * cff_value list * cff_key) decoder =
  let rec aux stepsum vacc =
    d_dict_element >>= fun (step, elem) ->
      match elem with
      | Value(v) -> aux (stepsum + step) (Alist.extend vacc v)
      | Key(k)   -> return (stepsum + step, Alist.to_list vacc, k)
  in
  aux 0 Alist.empty


let d_dict len : dict decoder =
  let rec aux mapacc len =
    if len = 0 then
      return mapacc
    else if len < 0 then
      err InconsistentDictLength
    else
      d_dict_keyval >>= fun (step, vs, k) ->
      aux (mapacc |> DictMap.add k vs) (len - step)
  in
  aux DictMap.empty len


let seek_required_table table_directory tag =
  let open ResultMonad in
  match table_directory |> TableDirectory.find_opt tag with
  | None    -> err @@ Error.MissingRequiredTable(tag)
  | Some(v) -> return @@ v


let seek_table table_directory tag =
  table_directory |> TableDirectory.find_opt tag
