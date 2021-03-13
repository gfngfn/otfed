
open Basic
open Value
open DecodeBasic
open DecodeOperation


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


module GxxxScheme = struct

  include GeneralTable

  type script = {
    script_source             : t;
    script_tag                : string;
    script_offset_Script      : offset;
    script_offset_FeatureList : offset;
    script_offset_LookupList  : offset;
  }

  type langsys = {
    langsys_source             : t;
    langsys_tag                : string;
    langsys_offset_LangSys     : offset;
    langsys_offset_FeatureList : offset;
    langsys_offset_LookupList  : offset;
  }

  type feature = {
    feature_source            : t;
    feature_tag               : string;
    feature_offset_Feature    : offset;
    feature_offset_LookupList : offset;
  }


  let get_script_tag script =
    script.script_tag


  let get_langsys_tag langsys =
    langsys.langsys_tag


  let get_feature_tag feature =
    feature.feature_tag


  let d_tag_and_offset_record (offset_ScriptList : offset) : (string * offset) decoder =
    d_bytes 4 >>= fun tag ->
    d_offset offset_ScriptList >>= fun offset ->
    return (tag, offset)


  let d_tag_and_offset_list : ((string * int) list) decoder =
    current >>= fun offset_ScriptList ->
    d_list (d_tag_and_offset_record offset_ScriptList)


  let scripts (gxxx : t) : (script list) ok =
    let offset_Gxxx = gxxx.offset in
    let dec =
      d_uint32 >>= fun version ->
      if version <> !%% 0x00010000L then
        err @@ Error.UnknownTableVersion(version)
      else
        d_fetch offset_Gxxx d_tag_and_offset_list >>= fun scriptList ->
        d_offset offset_Gxxx >>= fun offset_FeatureList ->
        d_offset offset_Gxxx >>= fun offset_LookupList ->
      let scripts =
        scriptList |> List.map (fun (scriptTag, offset_Script) ->
          {
            script_source             = gxxx;
            script_tag                = scriptTag;
            script_offset_Script      = offset_Script;
            script_offset_FeatureList = offset_FeatureList;
            script_offset_LookupList  = offset_LookupList;
          }
        )
      in
      return scripts
    in
    dec |> DecodeOperation.run gxxx.core offset_Gxxx


  let langsyses (script : script) : (langsys option * langsys list) ok =
    let gxxx               = script.script_source in
    let offset_Script      = script.script_offset_Script in
    let offset_FeatureList = script.script_offset_FeatureList in
    let offset_LookupList  = script.script_offset_LookupList in
    let dec =
      d_offset_opt offset_Script >>= fun offset_DefaultLangSys_opt ->
      d_list (d_tag_and_offset_record offset_Script) >>= fun langSysList ->
      let default_langsys_opt =
        offset_DefaultLangSys_opt |> Option.map (fun offset_DefaultLangSys ->
          {
            langsys_source             = gxxx;
            langsys_tag                = "DFLT";
            langsys_offset_LangSys     = offset_DefaultLangSys;
            langsys_offset_FeatureList = offset_FeatureList;
            langsys_offset_LookupList  = offset_LookupList;
          }
        )
      in
      let langsyses =
        langSysList |> List.map (fun (langSysTag, offset_LangSys) ->
          {
            langsys_source             = gxxx;
            langsys_tag                = langSysTag;
            langsys_offset_LangSys     = offset_LangSys;
            langsys_offset_FeatureList = offset_FeatureList;
            langsys_offset_LookupList  = offset_LookupList;
          }
        )
      in
      return (default_langsys_opt, langsyses)
    in
    dec |> DecodeOperation.run gxxx.core offset_Script


  module FeatureIndexSet = Set.Make(Int)


  let features (langsys : langsys) : (feature option * feature list) ok =
    let gxxx               = langsys.langsys_source in
    let offset_LangSys     = langsys.langsys_offset_LangSys in
    let offset_FeatureList = langsys.langsys_offset_FeatureList in
    let offset_LookupList  = langsys.langsys_offset_LookupList in
    let decLangSys =
      (* The position is set to the beginning of a LangSys table [page 134]. *)
      d_uint16 >>= fun lookupOrder ->
      if lookupOrder <> 0 then
        err @@ Error.UnknownLookupOrder(lookupOrder)
      else
        d_uint16 >>= fun requiredFeatureIndex ->
        d_list d_uint16 >>= fun featureIndices ->
        return (requiredFeatureIndex, FeatureIndexSet.of_list featureIndices)
    in
    let decFeature (requiredFeatureIndex, featureIndexSet) =
      d_list_filtered
        (d_tag_and_offset_record offset_FeatureList)
        (fun i -> FeatureIndexSet.mem i featureIndexSet) >>= fun featureList ->
      let features =
        featureList |> List.map (fun (featureTag, offset_Feature) ->
          {
            feature_source            = gxxx;
            feature_tag               = featureTag;
            feature_offset_Feature    = offset_Feature;
            feature_offset_LookupList = offset_LookupList;
          }
        )
      in
      begin
        match requiredFeatureIndex with
        | 0xFFFF ->
            return None

        | _ ->
            let dec =
              d_tag_and_offset_record offset_FeatureList >>= fun pair ->
              return @@ Some(pair)
            in
            pick (offset_FeatureList + 6 * requiredFeatureIndex) dec
            (* 6 is the size of FeatureRecord [page 135]. *)
      end >>= fun tag_and_offset_opt ->
      let required_feature_opt =
        tag_and_offset_opt |> Option.map (fun (tag, offset) ->
          {
            feature_source            = gxxx;
            feature_tag               = tag;
            feature_offset_Feature    = offset;
            feature_offset_LookupList = offset_LookupList;
          }
        )
      in
      return (required_feature_opt, features)
    in
    let open ResultMonad in
    decLangSys |> run gxxx.core offset_LangSys >>= fun pair ->
    (decFeature pair) |> run gxxx.core offset_FeatureList


  module LookupListIndexSet = Set.Make(Int)


  let subtables_scheme : 'a. 'a decoder -> feature -> ('a list) ok =
  fun lookup feature ->
    let gxxx              = feature.feature_source in
    let offset_Feature    = feature.feature_offset_Feature in
    let offset_LookupList = feature.feature_offset_LookupList in
    let dec =
      (* The position is set to the beginning of a Feature table. *)
      d_uint16 >>= fun _featureParams ->
      d_list d_uint16 >>= fun lookupListIndexList ->
      let lookupListIndexSet = LookupListIndexSet.of_list lookupListIndexList in
      d_list_filtered
        (d_offset offset_LookupList)
        (fun i -> LookupListIndexSet.mem i lookupListIndexSet) >>= fun offsets ->
      pick_each offsets lookup
    in
    dec |> run gxxx.core offset_Feature

end


module Gsub = struct

  include GxxxScheme


  type gsub_subtable =
    | SingleSubtable    of (glyph_id * glyph_id) list
        (* LookupType 1: Single substitution subtable [page 251] *)
    | AlternateSubtable of (glyph_id * (glyph_id list)) list
        (* LookupType 3: Alternate substitution subtable [page 253] *)
    | LigatureSubtable  of (glyph_id * (glyph_id list * glyph_id) list) list
        (* LookupType 4: Ligature substitution subtable [page 254] *)
    | Unsupported


  let d_single_substitution_subtable_format_1 (offset_Substitution : offset) =
    d_fetch offset_Substitution d_coverage >>= fun coverage ->
    d_uint16 >>= fun deltaGlyphID ->
    return (coverage |> List.map (fun gid -> (gid, gid + deltaGlyphID)))


  let d_single_substitution_subtable_format_2 (offset_Substitution : offset) =
    d_fetch_coverage_and_values offset_Substitution d_uint16


  let d_single_substitution_subtable : ((glyph_id * glyph_id) list) decoder =
    (* The position is supposed to be set to the beginning of
       a Single SubstFormat1 or a Single SubstFormat2 subtable [page 251]. *)
    current >>= fun offset_Substitution ->
    d_uint16 >>= fun substFormat ->
    match substFormat with
    | 1 -> d_single_substitution_subtable_format_1 offset_Substitution
    | 2 -> d_single_substitution_subtable_format_2 offset_Substitution
    | _ -> err @@ Error.UnknownFormatNumber(substFormat)


  let d_alternate_set_table =
    d_list d_uint16


  let d_alternate_substitution_subtable : ((glyph_id * glyph_id set) list) decoder =
    current >>= fun offset_Substitution ->
    d_uint16 >>= fun substFormat ->
    match substFormat with
    | 1 -> d_fetch_coverage_and_values offset_Substitution d_alternate_set_table
    | _ -> err @@ Error.UnknownFormatNumber(substFormat)


  let lookup_gsub =
    current >>= fun offset_Lookup ->
    d_uint16 >>= fun lookupType ->
    d_uint16 >>= fun _lookupFlag ->
    match lookupType with
    | 1 ->
        d_list (d_fetch offset_Lookup d_single_substitution_subtable) >>= fun assocs ->
        return (SingleSubtable(List.concat assocs))

    | 3 ->
        d_list (d_fetch offset_Lookup d_alternate_substitution_subtable) >>= fun assocs ->
        return (AlternateSubtable(List.concat assocs))

    | 2 | 4 | 5 | 6 | 7 | 8 ->
        err @@ Error.Unsupported(Error.UnsupportedGsubLookupType(lookupType))

    | _ ->
        err @@ Error.UnknownGsubLookupType(lookupType)

end
