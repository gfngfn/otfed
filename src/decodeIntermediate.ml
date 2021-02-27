
open Basic
open DecodeBasic


module GeneralTable = struct

  type t = {
    core   : common_source_core;
    offset : offset;
    length : int;
  }


  let make (core : common_source_core) (offset : offset) (length : int) : t =
    {
      core   = core;
      offset = offset;
      length = length;
    }

end


module Cmap = struct

  include GeneralTable


  type subtable = t * offset * Value.Cmap.subtable_ids


  open DecodeOperation


  let d_encoding_record (cmap : t) : subtable decoder =
    d_uint16 >>= fun platform_id ->
    d_uint16 >>= fun encoding_id ->
    d_fetch_long cmap.offset d_uint16 >>= fun (offset, format) ->
    let ids =
      Value.Cmap.{
        platform_id = platform_id;
        encoding_id = encoding_id;
        format      = format;
      }
    in
    return @@ (cmap, offset, ids)


  let get_subtables (cmap : t) : (subtable set) ok =
    let open DecodeOperation in
    let dec =
      d_uint16 >>= fun version ->
      if version <> 0 then
        err @@ UnknownTableVersion(!% version)
      else
        d_list (d_encoding_record cmap) >>= fun raw_subtables ->
        let subtables =
          raw_subtables |> List.filter (fun (_, _, ids) ->
            let open Value.Cmap in
            match ids.format with
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


  let get_subtable_ids ((_, _, ids) : subtable) =
    ids


  let uchar_of_int n =
    let open ResultMonad in
    try return @@ Uchar.of_int n with
    | _ -> err @@ Error.InvalidCodePoint(n)


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
                let acc = f acc (Incremental(uchFirst1, uchLast1, gidFirst1)) in
                let acc = f acc (Incremental(uchFirst2, uchLast2, 0)) in
                return acc
              else if gidStart <= 65535 && 65535 < gidEnd then
                transform_result (uchar_of_int startCode) >>= fun uchFirst1 ->
                transform_result (uchar_of_int (65535 - idDelta)) >>= fun uchLast1 ->
                transform_result (uchar_of_int (65536 - idDelta)) >>= fun uchFirst2 ->
                transform_result (uchar_of_int endCode) >>= fun uchLast2 ->
                let gidFirst1 = gidStart in
                let acc = f acc (Incremental(uchFirst1, uchLast1, gidFirst1)) in
                let acc = f acc (Incremental(uchFirst2, uchLast2, 0)) in
                return acc
              else
                transform_result (uchar_of_int startCode) >>= fun uchStart ->
                transform_result (uchar_of_int endCode) >>= fun uchEnd ->
                let acc = f acc (Incremental(uchStart, uchEnd, gidStart land 65535)) in
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


  let d_cmap_4 f acc =
    (* Position: immediately AFTER the format number entry of a cmap subtable. *)
    d_skip (2 * 2) >>= fun () -> (* Skips `length` and `language`. *)
    d_uint16 >>= fun segCountX2 ->
    let segCount = segCountX2 / 2 in
    d_skip (2 * 3) >>= fun () -> (* Skips `searchRange`, `entrySelector`, and `rangeShift`. *)
    d_repeat segCount d_uint16 >>= fun endCodes ->
    d_skip 1 >>= fun () -> (* Skips a reserved pad. *)
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
        let acc = f acc (k startCharCode endCharCode startGlyphId) in
        d_cmap_groups k (count - 1) f acc


  let d_cmap_segment k f acc =
    (* Position: immediately AFTER the format number entry of a cmap subtable *)
    d_skip (1 * 2 + 2 * 4) >>= fun () ->
    d_uint32_int >>= fun nGroups ->
    d_cmap_groups k nGroups f acc


  let d_cmap_12 f =
    d_cmap_segment (fun startCharCode endCharCode startGlyphId ->
      Incremental(startCharCode, endCharCode, startGlyphId)
    ) f


  let d_cmap_13 f =
    d_cmap_segment (fun startCharCode endCharCode startGlyphId ->
      Constant(startCharCode, endCharCode, startGlyphId)
    ) f


  let fold_subtable (subtable : subtable) f acc =
    let (cmap, offset, _) = subtable in
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

end
