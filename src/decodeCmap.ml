
open Basic
open DecodeBasic
open DecodeOperation.Open


include GeneralTable(struct type t = unit end)


let get (src : source) : t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_cmap >>= fun (offset, length) ->
  return @@ make_scheme common.core offset length ()


type format = int

type subtable = t * offset * Value.Cmap.subtable_ids * format


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


let get_subtables (cmap : t) : (subtable set) ok =
  let open DecodeOperation in
  let dec =
    d_uint16 >>= fun version ->
    if version <> 0 then
      err @@ UnknownTableVersion(!% version)
    else
      d_list (d_encoding_record cmap) >>= fun raw_subtables ->
      let subtables =
        raw_subtables |> List.filter (fun (_, _, ids, format) ->
          let open Value.Cmap in
          match format with
          | 4 | 12 | 13 ->
              begin
                match (ids.platform_id, ids.encoding_id) with
                | (0, _)  (* Unicode *)
                | (3, 1)  (* Windows, UCS-2 *)
                | (3, 10) (* Windows, UCS-4 *)
                | (1, _)  (* Macintosh *)
                    -> true

                | _ -> false
              end

          | _ ->
              false
        )
      in
      return subtables
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


let make_incremental_segment ~msg ~start:startCharCode ~last:endCharCode ~gid:startGlyphId =
  let open DecodeOperation in
  if startCharCode > endCharCode then
    err @@ Error.InvalidCmapSegment{
      msg; (* for debug *)
      incremental    = true;
      start_char     = startCharCode;
      end_char       = endCharCode;
      start_glyph_id = startGlyphId;
    }
  else
    return @@ Incremental(startCharCode, endCharCode, startGlyphId)


let d_cmap_4_loop (offset_glyphIdArray : offset) (segCount : int) (f : 'a -> cmap_segment -> 'a) =
  let rec aux i acc = function
    | ([], [], [], []) ->
        assert (i = segCount);
        return acc

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
              make_incremental_segment ~msg:"1a" ~start:uchFirst1 ~last:uchLast1 ~gid:gidFirst1 >>= fun segment1 ->
              let acc = f acc segment1 in
              make_incremental_segment ~msg:"1b" ~start:uchFirst2 ~last:uchLast2 ~gid:0 >>= fun segment2 ->
              let acc = f acc segment2 in
              return acc
            else if gidStart <= 65535 && 65535 < gidEnd then
              transform_result (uchar_of_int startCode) >>= fun uchFirst1 ->
              transform_result (uchar_of_int (65535 - idDelta)) >>= fun uchLast1 ->
              transform_result (uchar_of_int (65536 - idDelta)) >>= fun uchFirst2 ->
              transform_result (uchar_of_int endCode) >>= fun uchLast2 ->
              let gidFirst1 = gidStart in
              make_incremental_segment ~msg:"2a" ~start:uchFirst1 ~last:uchLast1 ~gid:gidFirst1 >>= fun segment1 ->
              let acc = f acc segment1 in
              make_incremental_segment ~msg:"2b" ~start:uchFirst2 ~last:uchLast2 ~gid:0 >>= fun segment2 ->
              let acc = f acc segment2 in
              return acc
            else
              transform_result (uchar_of_int startCode) >>= fun uchStart ->
              transform_result (uchar_of_int endCode) >>= fun uchEnd ->
              make_incremental_segment ~msg:"3" ~start:uchStart ~last:uchEnd ~gid:(gidStart land 65535) >>= fun segment ->
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
        assert false
  in
  aux 0


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
    make_incremental_segment ~msg:"format 12" ~start ~last ~gid
  ) f


let d_cmap_13 f =
  d_cmap_segment (fun startCharCode endCharCode startGlyphId ->
    if startCharCode > endCharCode then
      err @@ Error.InvalidCmapSegment{
        msg = "format 13"; (* for debug *)
        incremental    = false;
        start_char     = startCharCode;
        end_char       = endCharCode;
        start_glyph_id = startGlyphId;
      }
    else
      return @@ Constant(startCharCode, endCharCode, startGlyphId)
  ) f


let fold_subtable (subtable : subtable) f acc =
  let (cmap, offset, _, _) = subtable in
  let dec =
    (* Position: at the beginning of the designated cmap subtable *)
    d_uint16 >>= fun format ->
    match format with
    | 4  -> d_cmap_4 f acc
    | 12 -> d_cmap_12 f acc
    | 13 -> d_cmap_13 f acc
    | _  -> err @@ UnsupportedCmapFormat(format)
  in
  run cmap.core offset dec

(*
let to_value (subtable : subtable) =
  fold_subtable subtable (fun map segment ->
    match segment with
    | _ -> ()
  ) Value.Cmap.Mapping.empty
*)
