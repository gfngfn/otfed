
open Basic
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


let d_offsize : offsize decoder =
  d_uint8 >>= function
  | 1 -> return OffSize1
  | 2 -> return OffSize2
  | 3 -> return OffSize3
  | 4 -> return OffSize4
  | n -> err @@ Error.InvalidOffsize(n)


let seek_required_table table_directory tag =
  let open ResultMonad in
  match table_directory |> TableDirectory.find_opt tag with
  | None    -> err @@ Error.MissingRequiredTable(tag)
  | Some(v) -> return @@ v
