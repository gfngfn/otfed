
open Basic


include DecodeOperationCore


let d_uint32_int : int decoder =
  d_uint32 >>= fun n ->
  return @@ WideInt.to_int n


let d_repeat : 'a. int -> 'a decoder -> ('a list) decoder =
fun count d ->
  let rec aux acc i =
    if i <= 0 then
      return @@ Alist.to_list acc
    else
      d >>= fun v ->
      aux (Alist.extend acc v) (i - 1)
  in
  aux Alist.empty count


let d_tag : Value.Tag.t decoder =
  d_uint32 >>= fun n ->
  return @@ Value.Tag.of_wide_int n


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
