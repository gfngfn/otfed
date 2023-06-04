
open Basic
open DecodeBasic
open DecodeOperation.Open


include GeneralTable(struct type t = unit end)


let get (src : source) : t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_cmap >>= fun (offset, length) ->
  return @@ make_scheme common.core offset length ()


type segment =
  | Incremental of Uchar.t * Uchar.t * Value.glyph_id
  | Constant    of Uchar.t * Uchar.t * Value.glyph_id


let pp_segment ppf = function
  | Incremental(uch1, uch2, gid) ->
      Format.fprintf ppf "Incremental(%a, %a, %d)" pp_uchar uch1 pp_uchar uch2 gid

  | Constant(uch1, uch2, gid) ->
      Format.fprintf ppf "Constant(%a, %a, %d)" pp_uchar uch1 pp_uchar uch2 gid


type format = int

type subtable = t * offset * Value.Cmap.subtable_ids * format

type variation_subtable = subtable


open DecodeOperation


let d_encoding_record (cmap : t) : subtable decoder =
  d_uint16 >>= fun platform_id ->
  d_uint16 >>= fun encoding_id ->
  d_fetch_long cmap.offset d_uint16 >>= fun (offset, format) ->
  let ids =
    Value.Cmap.{
      platform_id = platform_id;
      encoding_id = encoding_id;
    }
  in
  return @@ (cmap, offset, ids, format)


let get_subtables (cmap : t) : (subtable set * variation_subtable set) ok =
  let open DecodeOperation in
  let dec =
    d_uint16 >>= fun version ->
    if version <> 0 then
      err @@ UnknownTableVersion(!% version)
    else
      d_list (d_encoding_record cmap) >>= fun raw_subtables ->
      let (ordinary_acc, variation_acc) =
        raw_subtables |> List.fold_left (fun (ordinary_acc, variation_acc) raw_subtable ->
          let open Value.Cmap in
          let (_, _, ids, format) = raw_subtable in
          match format with
          | 4 | 12 | 13 ->
              begin
                match (ids.platform_id, ids.encoding_id) with
                | (0, _)    (* Unicode *)
                | (3, 1)    (* Windows, UCS-2 *)
                | (3, 10)   (* Windows, UCS-4 *)
                | (1, _) -> (* Macintosh *)
                    (Alist.extend ordinary_acc raw_subtable, variation_acc)

                | _ ->
                    (ordinary_acc, variation_acc)
              end

          | 14 ->
              (ordinary_acc, Alist.extend variation_acc raw_subtable)

          | _ ->
              (ordinary_acc, variation_acc)

        ) (Alist.empty, Alist.empty)
      in
      return (Alist.to_list ordinary_acc, Alist.to_list variation_acc)
  in
  run cmap.core cmap.offset dec


let get_subtable_ids ((_, _, ids, _) : subtable) =
  ids


let get_format_number ((_, _, _, format) : subtable) =
  format


let uchar_of_int n =
  let open ResultMonad in
  try return @@ Uchar.of_int n with
  | _ -> err @@ Error.InvalidCodePoint(n)


let make_segment ~incremental ~start:startCharCode ~last:endCharCode ~gid:startGlyphId =
  let open DecodeOperation in
  if startCharCode > endCharCode then
    err @@ Error.InvalidCmapSegment{
      incremental    = incremental;
      start_char     = startCharCode;
      end_char       = endCharCode;
      start_glyph_id = startGlyphId;
    }
  else if incremental then
    return @@ Incremental(startCharCode, endCharCode, startGlyphId)
  else
    return @@ Constant(startCharCode, endCharCode, startGlyphId)


let d_cmap_4_loop (offset_glyphIdArray : offset) (segCount : int) (f : 'a -> segment -> 'a) acc (endCodes, startCodes, idDeltas, idRangeOffsets) =
  let rec aux i acc = function
    | ([], [], [], []) ->
        if i = segCount then
          return acc
        else
          err @@ Error.InconsistentNumberOfCmapSegments{
            seg_count            = segCount;
            num_end_codes        = List.length endCodes;
            num_start_codes      = List.length startCodes;
            num_id_deltas        = List.length idDeltas;
            num_id_range_offsets = List.length idRangeOffsets;
          }

    | (
        endCode :: endCodes,
        startCode :: startCodes,
        idDelta :: idDeltas,
        idRangeOffset :: idRangeOffsets
      ) ->
        begin
          if idRangeOffset = 0 then
            let gidEnd = endCode + idDelta in
            let gidStart = startCode + idDelta in
            if gidStart < 0 && 0 <= gidEnd then
              transform_result (uchar_of_int startCode) >>= fun uchFirst1 ->
              transform_result (uchar_of_int (- idDelta - 1)) >>= fun uchLast1 ->
              transform_result (uchar_of_int (- idDelta)) >>= fun uchFirst2 ->
              transform_result (uchar_of_int endCode) >>= fun uchLast2 ->
              let gidFirst1 = gidStart land 65535 in
              make_segment ~incremental:true ~start:uchFirst1 ~last:uchLast1 ~gid:gidFirst1 >>= fun segment1 ->
              let acc = f acc segment1 in
              make_segment ~incremental:true ~start:uchFirst2 ~last:uchLast2 ~gid:0 >>= fun segment2 ->
              let acc = f acc segment2 in
              return acc
            else if gidStart <= 65535 && 65535 < gidEnd then
              transform_result (uchar_of_int startCode) >>= fun uchFirst1 ->
              transform_result (uchar_of_int (65535 - idDelta)) >>= fun uchLast1 ->
              transform_result (uchar_of_int (65536 - idDelta)) >>= fun uchFirst2 ->
              transform_result (uchar_of_int endCode) >>= fun uchLast2 ->
              let gidFirst1 = gidStart in
              make_segment ~incremental:true ~start:uchFirst1 ~last:uchLast1 ~gid:gidFirst1 >>= fun segment1 ->
              let acc = f acc segment1 in
              make_segment ~incremental:true ~start:uchFirst2 ~last:uchLast2 ~gid:0 >>= fun segment2 ->
              let acc = f acc segment2 in
              return acc
            else
              transform_result (uchar_of_int startCode) >>= fun uchStart ->
              transform_result (uchar_of_int endCode) >>= fun uchEnd ->
              make_segment ~incremental:true ~start:uchStart ~last:uchEnd ~gid:(gidStart land 65535) >>= fun segment ->
              let acc = f acc segment in
              return acc
          else
            let rec iter cp acc =
              if endCode < cp then
                return acc
              else
                (* Doubtful: how to calculate `index` *)
                let index = cp - startCode + idRangeOffset / 2 + i - segCount in
                pick (offset_glyphIdArray + 2 * index) d_uint16 >>= fun g ->
                if g = 0 then
                  iter (cp + 1) acc
                else
                  let gid = (g + idDelta) land 65535 in
                  transform_result (uchar_of_int cp) >>= fun uch ->
                  let acc = f acc (Constant(uch, uch, gid)) in
                  iter (cp + 1) acc
            in
            iter startCode acc
        end >>= fun acc ->
        aux (i + 1) acc (endCodes, startCodes, idDeltas, idRangeOffsets)

    | _ ->
        err @@ Error.InconsistentNumberOfCmapSegments{
          seg_count            = segCount;
          num_end_codes        = List.length endCodes;
          num_start_codes      = List.length startCodes;
          num_id_deltas        = List.length idDeltas;
          num_id_range_offsets = List.length idRangeOffsets;
        }
  in
  aux 0 acc (endCodes, startCodes, idDeltas, idRangeOffsets)


let d_reserved_pad =
  d_uint16 >>= fun n ->
  if n = 0 then
    return ()
  else
    err @@ Error.InvalidReservedPad(n)


let d_cmap_4 f acc =
  (* Position: immediately AFTER the format number entry of a cmap subtable. *)
  d_skip (2 * 2) >>= fun () -> (* Skips `length` and `language`. *)
  d_uint16 >>= fun segCountX2 ->
  let segCount = segCountX2 / 2 in
  d_skip (2 * 3) >>= fun () -> (* Skips `searchRange`, `entrySelector`, and `rangeShift`. *)
  d_repeat segCount d_uint16 >>= fun endCodes ->
  d_reserved_pad >>= fun () ->
  d_repeat segCount d_uint16 >>= fun startCodes ->
  d_repeat segCount d_int16 >>= fun idDeltas ->
  d_repeat segCount d_uint16 >>= fun idRangeOffsets ->
  current >>= fun offset_glyphIdArray ->
  d_cmap_4_loop offset_glyphIdArray segCount f acc (endCodes, startCodes, idDeltas, idRangeOffsets)


let rec d_cmap_groups k count f acc =
  if count <= 0 then
    return acc
  else
    d_code_point >>= fun startCharCode ->
    d_code_point >>= fun endCharCode ->
    if startCharCode > endCharCode then
      err @@ InvalidCodePointRange(startCharCode, endCharCode)
    else
      d_uint32_int >>= fun startGlyphId ->
      k startCharCode endCharCode startGlyphId >>= fun segment ->
      let acc = f acc segment in
      d_cmap_groups k (count - 1) f acc


let d_cmap_segment k f acc =
  (* Position: immediately AFTER the format number entry of a cmap subtable *)
  d_skip (1 * 2 + 2 * 4) >>= fun () ->
  d_uint32_int >>= fun nGroups ->
  d_cmap_groups k nGroups f acc


let d_cmap_12 f =
  d_cmap_segment (fun start last gid ->
    make_segment ~incremental:true ~start ~last ~gid
  ) f


let d_cmap_13 f =
  d_cmap_segment (fun start last gid ->
    make_segment ~incremental:false ~start ~last ~gid
  ) f


let d_cmap_subtable f acc =
  let open DecodeOperation in
  (* Position: at the beginning of the designated cmap subtable *)
  d_uint16 >>= fun format ->
  match format with
  | 4  -> d_cmap_4 f acc
  | 12 -> d_cmap_12 f acc
  | 13 -> d_cmap_13 f acc
  | _  -> err @@ UnsupportedCmapFormat(format)


let fold_subtable (subtable : subtable) f acc =
  let (cmap, offset, _, _) = subtable in
  run cmap.core offset (d_cmap_subtable f acc)


let unmarshal_subtable (subtable : subtable) : Value.Cmap.subtable ok =
  let open ResultMonad in
  let open Value.Cmap in
  fold_subtable subtable (fun mapping segment ->
    match segment with
    | Incremental(uch1, uch2, gid) ->
        mapping |> Mapping.add_incremental_range ~start:uch1 ~last:uch2 ~gid:gid

    | Constant(uch1, uch2, gid) ->
        mapping |> Mapping.add_constant_range ~start:uch1 ~last:uch2 ~gid:gid

  ) Mapping.empty >>= fun mapping ->
  return Value.Cmap.{
    subtable_ids = get_subtable_ids subtable;
    mapping;
  }


type 'a folding_variation_entry = {
  folding_default     : Uchar.t -> 'a -> 'a;
  folding_non_default : Uchar.t -> Value.glyph_id -> 'a -> 'a;
}


let d_default_uvs_table _folding_default _acc =
  failwith "TODO: d_default_uvs_table"


let d_non_default_uvs_table _folding_non_default _acc =
  failwith "TODO: d_non_default_uvs_table"


type variation_selector_record = {
  var_selector           : Uchar.t;
  default_uvs_offset     : offset;
  non_default_uvs_offset : offset;
}


let d_variation_selector_record (offset_varsubtable : offset) : variation_selector_record decoder =
  let open DecodeOperation in
  d_uint24 >>= fun varSelector ->
  d_long_offset offset_varsubtable >>= fun default_uvs_offset ->
  d_long_offset offset_varsubtable >>= fun non_default_uvs_offset ->
  return { var_selector = Uchar.of_int varSelector; default_uvs_offset; non_default_uvs_offset }


let d_cmap_14 (offset_varsubtable : offset) (f : Uchar.t -> 'a folding_variation_entry) (acc : 'a) : 'a decoder =
  let open DecodeOperation in
  (* Position: immediately AFTER the format number entry of a cmap subtable *)
  d_uint32 >>= fun _length ->
  d_uint32_int >>= fun numVarSelectorRecords ->
  d_repeat numVarSelectorRecords (d_variation_selector_record offset_varsubtable) >>= fun records ->
  foldM (fun acc record ->
    let { var_selector; default_uvs_offset; non_default_uvs_offset } = record in
    let { folding_default; folding_non_default } = f var_selector in
    pick default_uvs_offset (d_default_uvs_table folding_default acc) >>= fun acc ->
    pick non_default_uvs_offset (d_non_default_uvs_table folding_non_default acc) >>= fun acc ->
    return acc
  ) records acc


let d_cmap_variation_subtable f acc =
  let open DecodeOperation in
  current >>= fun offset_varsubtable ->
  d_uint16 >>= fun format ->
  match format with
  | 14 -> d_cmap_14 offset_varsubtable f acc
  | _  -> err @@ UnsupportedCmapFormat(format)


let fold_variation_subtable (varsubtable : variation_subtable) f acc =
  let (cmap, offset, _, _) = varsubtable in
  run cmap.core offset (d_cmap_variation_subtable f acc)
