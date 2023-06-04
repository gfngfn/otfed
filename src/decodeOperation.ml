
open Basic
open Value
open DecodeBasic


include DecodeOperationCore

open Open


let d_uint32_int : int decoder =
  d_uint32 >>= fun n ->
  return @@ WideInt.to_int n


let d_int32_int : int decoder =
  d_uint32_int >>= fun n ->
  if n >= (1 lsl 31) then
    return @@ n - (1 lsl 32)
  else
    return n


let d_code_point : Uchar.t decoder =
  d_uint32_int >>= fun n ->
  if Uchar.is_valid n then
    return @@ Uchar.of_int n
  else
    err @@ Error.InvalidCodePoint(n)


let d_bmp_code_point : Uchar.t decoder =
  d_uint16 >>= fun n ->
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


let d_long_offset (offset_origin : offset) : offset decoder =
  d_uint32_int >>= fun reloffset ->
  return (offset_origin + reloffset)


let d_offset_opt (offset_origin : int) : (offset option) decoder =
  d_uint16 >>= fun reloffset ->
  if reloffset = 0 then
    return None
  else
    return @@ Some(offset_origin + reloffset)


let d_long_offset_opt (offset_origin : int) : (offset option) decoder =
  d_uint32_int >>= fun reloffset ->
  if reloffset = 0 then
    return None
  else
    return @@ Some(offset_origin + reloffset)


let d_fetch (offset_origin : offset) (dec : 'a decoder) : 'a decoder =
  d_offset offset_origin >>= fun offset ->
  pick offset dec


let d_fetch_opt (offset_origin : offset) (dec : 'a decoder) : ('a option) decoder =
  d_offset_opt offset_origin >>= function
  | None ->
      return None

  | Some(offset) ->
      pick offset dec >>= fun v ->
      return @@ Some(v)


let d_fetch_long (offset_origin : offset) (dec : 'a decoder) : (offset * 'a) decoder =
  current >>= fun pos_before ->
  d_long_offset offset_origin >>= fun offset ->
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


let d_fold : 'a 'b. int -> 'a decoder -> ('a -> 'b -> 'b) -> 'b -> 'b decoder =
fun count dec f acc ->
  let rec aux acc i =
    if i <= 0 then
      return acc
    else
      dec >>= fun v ->
      aux (f v acc) (i - 1)
  in
  aux acc count


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


let d_if cond dec =
  if cond then
    dec >>= fun res ->
    return @@ Some(res)
  else
    return None


let d_tag : Value.Tag.t decoder =
  d_uint32 >>= fun n ->
  return @@ Value.Tag.of_wide_int n


let d_loc_format : Intermediate.loc_format decoder =
  let open Intermediate in
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


let d_fetch_coverage_and_values (offset : int) (dec : 'a decoder) : ((glyph_id * 'a) list) decoder =
  (* The position is supposed to be set just before a Coverage field
     and a subsequent offset list [page 254 etc.] *)
  d_fetch offset d_coverage >>= fun coverage ->
  (* The position is set just before LigSetCount field [page 254] *)
  d_list (d_fetch offset dec) >>= fun vs ->
  combine_coverage coverage vs


let chop_two_bytes ~data ~unit_size ~repeat =
  let mask = 1 lsl unit_size in
  let half_mask = 1 lsl (unit_size - 1) in
  let max = 16 / unit_size in
  let rec aux ds i data =
    if i >= max then
      ds
    else
      let d =
        let u = data land (mask - 1) in
        if u >= half_mask then u - mask else u
      in
      let data = data lsr unit_size in
      aux (d :: ds) (i + 1) data
  in
  let ds = aux [] 0 data in
  ds |> List.mapi (fun i d -> (i, d)) |> List.filter_map (fun (i, d) -> if i < repeat then Some(d) else None)


let rec d_device_table_entries ~unit_size (i : int) (acc : int Alist.t) =
  let num_entries_per_16 = 16 / unit_size in
  if i <= 0 then
    return @@ Alist.to_list acc
  else
    d_uint16 >>= fun v ->
    if i < num_entries_per_16 then
      let ds = chop_two_bytes ~data:v ~unit_size ~repeat:i in
      return @@ Alist.to_list (Alist.append acc ds)
    else
      let ds = chop_two_bytes ~data:v ~unit_size ~repeat:num_entries_per_16 in
      d_device_table_entries ~unit_size (i - num_entries_per_16) (Alist.append acc ds)


let d_device : device decoder =
  d_uint16 >>= fun start_size ->
  d_uint16 >>= fun end_size ->
  d_uint16 >>= fun deltaFormat ->
  let n = end_size - start_size + 1 in
  begin
    match deltaFormat with
    | 1 -> d_device_table_entries ~unit_size:2 n Alist.empty
    | 2 -> d_device_table_entries ~unit_size:4 n Alist.empty
    | 3 -> d_device_table_entries ~unit_size:8 n Alist.empty
    | _ -> err @@ Error.UnknownFormatNumber(deltaFormat)
  end >>= fun delta_values ->
  return {
    start_size;
    delta_values;
  }


let seek_required_table table_directory tag =
  let open ResultMonad in
  match table_directory |> TableDirectory.find_opt tag with
  | None    -> err @@ Error.MissingRequiredTable(tag)
  | Some(v) -> return @@ v


let seek_table table_directory tag =
  table_directory |> TableDirectory.find_opt tag


let%test_unit "chop_two_bytes" =
  let cases =
    [
      (0b01_01_01_01_01_00_00_00, 2, 5, [1; 1; 1; 1; 1]);
      (0b00_01_10_11_00_00_00_00, 2, 4, [0; 1; -2; -1]);
      (0b0101_0101_0001_0000, 4, 3, [5; 5; 1]);
      (0b0101_0101_0001_0000, 4, 4, [5; 5; 1; 0]);
      (0b1111_1101_0001_0000, 4, 4, [-1; -3; 1; 0]);
      (0b11111111_00010000, 8, 2, [-1; 16]);
    ]
  in
  cases |> List.iter (fun (data, unit_size, repeat, expected) ->
    let got = chop_two_bytes ~data ~unit_size ~repeat in
    assert (List.equal Int.equal expected got)
  )
