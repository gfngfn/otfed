
open Basic
open Value
open DecodeBasic
open DecodeOperation.Open


module GeneralTable(Info : sig type t end) = struct

  type t = {
    core   : common_source_core;
    offset : offset;
    length : int;
    info   : Info.t;
  }


  let make_scheme (core : common_source_core) (offset : offset) (length : int) (info : Info.t) : t =
    {
      core   = core;
      offset = offset;
      length = length;
      info   = info;
    }


  let get_info (general_table : t) : Info.t =
    general_table.info

end


module Cmap = struct

  include GeneralTable(struct type t = unit end)


  let make core ~offset ~length =
    make_scheme core offset length ()


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


module Hmtx = struct

  type info = {
    num_glyphs    : int;
    num_h_metrics : int;
  }

  include GeneralTable(struct type t = info end)


  let make core ~offset ~length ~num_glyphs ~num_h_metrics =
    make_scheme core offset length { num_glyphs; num_h_metrics }


  let get (hmtx : t) (gid : glyph_id) : ((int * int) option) ok =
    let open ResultMonad in
    let info = get_info hmtx in
    if gid < 0 || info.num_glyphs <= gid then
      return None
    else
      let index = if gid >= info.num_h_metrics then info.num_h_metrics - 1 else gid in
      let dec =
        let open DecodeOperation in
        d_uint16 >>= fun advanceWidth ->
        d_int16 >>= fun lsb ->
        return @@ Some((advanceWidth, lsb))
      in
      dec |> DecodeOperation.run hmtx.core (hmtx.offset + 4 * index)

end



module GxxxScheme = struct

  include GeneralTable(struct type t = unit end)


  let make core ~offset ~length =
    make_scheme core offset length ()


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
    let open DecodeOperation in
    d_bytes 4 >>= fun tag ->
    d_offset offset_ScriptList >>= fun offset ->
    return (tag, offset)


  let d_tag_and_offset_list : ((string * int) list) decoder =
    let open DecodeOperation in
    current >>= fun offset_ScriptList ->
    d_list (d_tag_and_offset_record offset_ScriptList)


  let scripts (gxxx : t) : (script list) ok =
    let offset_Gxxx = gxxx.offset in
    let dec =
      let open DecodeOperation in
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
      let open DecodeOperation in
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
      let open DecodeOperation in
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
      let open DecodeOperation in
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
    decLangSys |> DecodeOperation.run gxxx.core offset_LangSys >>= fun pair ->
    (decFeature pair) |> DecodeOperation.run gxxx.core offset_FeatureList


  module LookupListIndexSet = Set.Make(Int)


  let subtables_scheme : 'a. 'a decoder -> feature -> ('a list) ok =
  fun lookup feature ->
    let gxxx              = feature.feature_source in
    let offset_Feature    = feature.feature_offset_Feature in
    let offset_LookupList = feature.feature_offset_LookupList in
    let decFeature =
      let open DecodeOperation in
      (* The position is set to the beginning of a Feature table. *)
      d_uint16 >>= fun _featureParams ->
      d_list d_uint16 >>= fun lookupListIndexList ->
      return @@ LookupListIndexSet.of_list lookupListIndexList
    in
    let decLookup lookupListIndexSet =
      let open DecodeOperation in
      d_list_filtered
        (d_offset offset_LookupList)
        (fun i -> LookupListIndexSet.mem i lookupListIndexSet) >>= fun offsets ->
      pick_each offsets lookup
    in
    let open ResultMonad in
    decFeature |> DecodeOperation.run gxxx.core offset_Feature >>= fun lookupListIndexSet ->
    (decLookup lookupListIndexSet) |> DecodeOperation.run gxxx.core offset_LookupList

end


module Gsub = struct

  include GxxxScheme


  type subtable =
    | SingleSubtable    of (glyph_id * glyph_id) list
        (* LookupType 1: Single substitution subtable [page 251] *)
    | AlternateSubtable of (glyph_id * (glyph_id list)) list
        (* LookupType 3: Alternate substitution subtable [page 253] *)
    | LigatureSubtable  of (glyph_id * (glyph_id list * glyph_id) list) list
        (* LookupType 4: Ligature substitution subtable [page 254] *)
    | UnsupportedSubtable


  let d_single_substitution_subtable_format_1 (offset_Substitution : offset) =
    let open DecodeOperation in
    d_fetch offset_Substitution d_coverage >>= fun coverage ->
    d_uint16 >>= fun deltaGlyphID ->
    return (coverage |> List.map (fun gid -> (gid, gid + deltaGlyphID)))


  let d_single_substitution_subtable_format_2 (offset_Substitution : offset) =
    let open DecodeOperation in
    d_fetch_coverage_and_values offset_Substitution d_uint16


  let d_single_substitution_subtable : ((glyph_id * glyph_id) list) decoder =
    let open DecodeOperation in
    (* The position is supposed to be set to the beginning of
       a Single SubstFormat1 or a Single SubstFormat2 subtable [page 251]. *)
    current >>= fun offset_Substitution ->
    d_uint16 >>= fun substFormat ->
    match substFormat with
    | 1 -> d_single_substitution_subtable_format_1 offset_Substitution
    | 2 -> d_single_substitution_subtable_format_2 offset_Substitution
    | _ -> err @@ Error.UnknownFormatNumber(substFormat)


  let d_alternate_set_table =
    let open DecodeOperation in
    d_list d_uint16


  let d_alternate_substitution_subtable : ((glyph_id * glyph_id set) list) decoder =
    let open DecodeOperation in
    current >>= fun offset_Substitution ->
    d_uint16 >>= fun substFormat ->
    match substFormat with
    | 1 -> d_fetch_coverage_and_values offset_Substitution d_alternate_set_table
    | _ -> err @@ Error.UnknownFormatNumber(substFormat)


  let d_ligature_table : (glyph_id list * glyph_id) decoder =
    let open DecodeOperation in
    (* The position is supposed to be set to the beginning of
       a Ligature table [page 255]. *)
    d_uint16 >>= fun ligGlyph ->
    d_uint16 >>= fun compCount ->
    d_repeat (compCount - 1) d_uint16 >>= fun component ->
    return (component, ligGlyph)


  let d_ligature_set_table : ((glyph_id list * glyph_id) list) decoder =
    let open DecodeOperation in
    (* The position is supposed to be set to the beginning of
       a LigatureSet table [page 254]. *)
    current >>= fun offset_LigatureSet ->
    d_list (d_fetch offset_LigatureSet d_ligature_table)


  let d_ligature_substitution_subtable : ((glyph_id * (glyph_id list * glyph_id) list) list) decoder =
    let open DecodeOperation in
    (* The position is supposed to be set to the beginning of
       a Ligature SubstFormat1 subtable [page 254]. *)
    current >>= fun offset_Substitution ->
    d_uint16 >>= fun substFormat ->
    match substFormat with
    | 1 -> d_fetch_coverage_and_values offset_Substitution d_ligature_set_table
    | _ -> err @@ Error.UnknownFormatNumber(substFormat)


  let lookup =
    let open DecodeOperation in
    current >>= fun offset_Lookup ->
    d_uint16 >>= fun lookupType ->
    d_uint16 >>= fun _lookupFlag ->
    match lookupType with
    | 1 ->
        d_list (d_fetch offset_Lookup d_single_substitution_subtable) >>= fun assocs ->
        return @@ SingleSubtable(List.concat assocs)

    | 3 ->
        d_list (d_fetch offset_Lookup d_alternate_substitution_subtable) >>= fun assocs ->
        return @@ AlternateSubtable(List.concat assocs)

    | 4 ->
        d_list (d_fetch offset_Lookup d_ligature_substitution_subtable) >>= fun assocs ->
        return @@ LigatureSubtable(List.concat assocs)

    | 2 | 5 | 6 | 7 | 8 ->
        return UnsupportedSubtable (* TODO *)

    | _ ->
        err @@ Error.UnknownGsubLookupType(lookupType)


  type 'a folding_single = 'a -> glyph_id * glyph_id -> 'a

  type 'a folding_alt = 'a -> glyph_id * glyph_id list -> 'a

  type 'a folding_lig = 'a -> glyph_id * (glyph_id list * glyph_id) list -> 'a


  let fold_subtables
      ?single:(f_single = (fun acc _ -> acc))
      ?alt:(f_alt = (fun acc _ -> acc))
      ?lig:(f_lig = (fun acc _ -> acc))
      (feature : feature) (acc : 'a) : 'a ok =
    let open ResultMonad in
    subtables_scheme lookup feature >>= fun subtables ->
    let acc =
      subtables |> List.fold_left (fun acc subtable ->
        match subtable with
        | SingleSubtable(assoc) ->
            List.fold_left f_single acc assoc

        | AlternateSubtable(assoc) ->
            List.fold_left f_alt acc assoc

        | LigatureSubtable(assoc) ->
            List.fold_left f_lig acc assoc

        | UnsupportedSubtable ->
            acc
      ) acc
    in
    return acc

end


module Gpos = struct

  include GxxxScheme


  type value_format = {
    format_x_placement  : bool;
    format_y_placement  : bool;
    format_x_advance    : bool;
    format_y_advance    : bool;
    format_x_pla_device : bool;
    format_y_pla_device : bool;
    format_x_adv_device : bool;
    format_y_adv_device : bool;
  }

  type value_record = {
    x_placement  : int option;
    y_placement  : int option;
    x_advance    : int option;
    y_advance    : int option;
    x_pla_device : int option;
    y_pla_device : int option;
    x_adv_device : int option;
    y_adv_device : int option;
  }


  let d_value_format : value_format decoder =
    let open DecodeOperation in
    d_uint16 >>= fun n ->
    return @@ {
      format_x_placement  = n land 1 > 0;
      format_y_placement  = n land 2 > 0;
      format_x_advance    = n land 4 > 0;
      format_y_advance    = n land 8 > 0;
      format_x_pla_device = n land 16 > 0;
      format_y_pla_device = n land 32 > 0;
      format_x_adv_device = n land 64 > 0;
      format_y_adv_device = n land 128 > 0;
    }


  let d_value_record (v : value_format) : value_record decoder =
    let open DecodeOperation in
    (* The position is supposed to be set to the beginning of a ValueRecord table [page 213]. *)
    d_if v.format_x_placement  d_int16 >>= fun xPlacement_opt ->
    d_if v.format_y_placement  d_int16 >>= fun yPlacement_opt ->
    d_if v.format_x_advance    d_int16 >>= fun xAdvance_opt ->
    d_if v.format_y_advance    d_int16 >>= fun yAdvance_opt ->
    d_if v.format_x_pla_device d_int16 >>= fun xPlaDevice_opt ->
    d_if v.format_y_pla_device d_int16 >>= fun yPlaDevice_opt ->
    d_if v.format_x_adv_device d_int16 >>= fun xAdvDevice_opt ->
    d_if v.format_y_adv_device d_int16 >>= fun yAdvDevice_opt ->
    return {
      x_placement  = xPlacement_opt;
      y_placement  = yPlacement_opt;
      x_advance    = xAdvance_opt;
      y_advance    = yAdvance_opt;
      x_pla_device = xPlaDevice_opt;
      y_pla_device = yPlaDevice_opt;
      x_adv_device = xAdvDevice_opt;
      y_adv_device = yAdvDevice_opt;
    }


  type device_table = int * int * int * int

  type anchor_adjustment =
    | NoAnchorAdjustment
    | AnchorPointAdjustment  of int
    | DeviceAnchorAdjustment of device_table * device_table

  type design_units = int

  type anchor = design_units * design_units * anchor_adjustment

  type mark_class = int

  type mark_record = mark_class * anchor

  type base_record = (anchor option) array
    (* Arrays of this type are indexed by `mark_class`.
       UNDOCUMENTED (in OpenType 1.8.3): BaseRecord tables sometimes contain NULL pointers. *)

  type component_record = (anchor option) array
    (* Arrays of this type are indexed by `mark_class`. *)

  type ligature_attach = component_record list

  type mark2_record = anchor array
    (* Arrays of this type are indexed by `mark_class`. *)

  type class_value = int
  [@@deriving show {with_path = false}]

  type class_definition =
    | GlyphToClass      of glyph_id * class_value
    | GlyphRangeToClass of glyph_id * glyph_id * class_value
  [@@deriving show {with_path = false}]

  type subtable =
    | SinglePosAdjustment1 of glyph_id list * value_record
    | SinglePosAdjustment2 of (glyph_id * value_record) list
    | PairPosAdjustment1   of (glyph_id * (glyph_id * value_record * value_record) list) list
    | PairPosAdjustment2   of class_definition list * class_definition list * (class_value * (class_value * value_record * value_record) list) list
    | MarkBasePos1         of int * (glyph_id * mark_record) list * (glyph_id * base_record) list
    | MarkLigPos1          of int * (glyph_id * mark_record) list * (glyph_id * ligature_attach) list
    | MarkMarkPos1         of int * (glyph_id * mark_record) list * (glyph_id * mark2_record) list


  let d_single_adjustment_subtable : subtable decoder =
    let open DecodeOperation in
    (* The position is supposed to be set to the beginning of a SinglePos subtable [page 192]. *)
    current >>= fun offset_SinglePos ->
    d_uint16 >>= fun posFormat ->
    d_fetch offset_SinglePos d_coverage >>= fun coverage ->
    match posFormat with
    | 1 ->
        d_value_format >>= fun valueFormat ->
        d_value_record valueFormat >>= fun valueRecord ->
        return (SinglePosAdjustment1(coverage, valueRecord))

    | 2 ->
        d_value_format >>= fun valueFormat ->
        d_list (d_value_record valueFormat) >>= fun singleposs ->
        combine_coverage coverage singleposs >>= fun comb ->
        return (SinglePosAdjustment2(comb))

    | _ ->
        err @@ Error.UnknownFormatNumber(posFormat)


  let numbering vs =
    vs |> List.mapi (fun i v -> (i, v))


  let d_class_2_record valfmt1 valfmt2 : (value_record * value_record) decoder =
    let open DecodeOperation in
    d_value_record valfmt1 >>= fun valrcd1 ->
    d_value_record valfmt2 >>= fun valrcd2 ->
    return (valrcd1, valrcd2)


  let d_class_1_record class2Count valfmt1 valfmt2 : ((class_value * value_record * value_record) list) decoder =
    let open DecodeOperation in
    d_repeat class2Count (d_class_2_record valfmt1 valfmt2) >>= fun pairs ->
    return (numbering pairs |> List.map (fun (x, (y, z)) -> (x, y, z)))


  let d_class : class_value decoder =
    let open DecodeOperation in
    d_uint16


  let d_class_definition_format_1 : (class_definition list) decoder =
    let open DecodeOperation in
    let rec aux acc gidstt lst =
      match lst with
      | []          -> return (Alist.to_list acc)
      | cls :: tail -> aux (Alist.extend acc (GlyphToClass(gidstt, cls))) (gidstt + 1) tail
    in
    d_uint16 >>= fun startGlyph ->
    d_list d_class >>= fun classValueArray ->
    aux Alist.empty startGlyph classValueArray


  let d_class_range_record : class_definition decoder =
    let open DecodeOperation in
    d_uint16 >>= fun start_gid ->
    d_uint16 >>= fun end_gid ->
    d_class >>= fun cls ->
    return (GlyphRangeToClass(start_gid, end_gid, cls))


  let d_class_definition_format_2 : (class_definition list) decoder =
    let open DecodeOperation in
    d_list d_class_range_record


  let d_class_definition : (class_definition list) decoder =
    let open DecodeOperation in
    (* The position is supposed to be set to the  beginning of a ClassDef table [page 140]. *)
    d_uint16 >>= fun classFormat ->
    match classFormat with
    | 1 -> d_class_definition_format_1
    | 2 -> d_class_definition_format_2
    | _ -> err @@ Error.UnknownFormatNumber(classFormat)


  let d_pair_value_record valfmt1 valfmt2 : (glyph_id * value_record * value_record) decoder =
    let open DecodeOperation in
    d_uint16 >>= fun secondGlyph ->
    d_value_record valfmt1 >>= fun value1 ->
    d_value_record valfmt2 >>= fun value2 ->
    return (secondGlyph, value1, value2)


  let d_pair_set valfmt1 valfmt2 : ((glyph_id * value_record * value_record) list) decoder =
    let open DecodeOperation in
    d_list (d_pair_value_record valfmt1 valfmt2)


  let d_pair_adjustment_subtable =
    let open DecodeOperation in
    (* The position is supposed to be set to the beginning of a PairPos subtable [page 194]. *)
    current >>= fun offset_PairPos ->
    d_uint16 >>= fun posFormat ->
    d_fetch offset_PairPos d_coverage >>= fun coverage ->
    match posFormat with
    | 1 ->
        d_value_format >>= fun valueFormat1 ->
        d_value_format >>= fun valueFormat2 ->
        d_list (d_fetch offset_PairPos (d_pair_set valueFormat1 valueFormat2)) >>= fun pairsets ->
        combine_coverage coverage pairsets >>= fun comb ->
        return @@ PairPosAdjustment1(comb)

    | 2 ->
        d_value_format >>= fun valueFormat1 ->
        d_value_format >>= fun valueFormat2 ->
        d_fetch offset_PairPos d_class_definition >>= fun classDef1 ->
        d_fetch offset_PairPos d_class_definition >>= fun classDef2 ->
        d_uint16 >>= fun class1Count ->
        d_uint16 >>= fun class2Count ->
        d_repeat class1Count (d_class_1_record class2Count valueFormat1 valueFormat2) >>= fun pairposs ->
        return @@ PairPosAdjustment2(classDef1, classDef2, numbering pairposs)

    | _ ->
        err @@ Error.UnknownFormatNumber(posFormat)


  let d_device_table : device_table decoder =
    let open DecodeOperation in
    d_uint16 >>= fun startSize ->
    d_uint16 >>= fun endSize ->
    d_uint16 >>= fun deltaFormat ->
    d_uint16 >>= fun deltaValue ->
    return (startSize, endSize, deltaFormat, deltaValue)


  let d_anchor : anchor decoder =
    let open DecodeOperation in
    d_uint16 >>= fun anchorFormat ->
    d_int16 >>= fun xcoord ->
    d_int16 >>= fun ycoord ->
    match anchorFormat with
    | 1 ->
        return (xcoord, ycoord, NoAnchorAdjustment)

    | 2 ->
        d_uint16 >>= fun anchorPoint ->
        return (xcoord, ycoord, AnchorPointAdjustment(anchorPoint))

    | 3 ->
        d_device_table >>= fun xdevtable ->
        d_device_table >>= fun ydevtable ->
        return (xcoord, ycoord, DeviceAnchorAdjustment(xdevtable, ydevtable))

    | _ ->
        err @@ Error.UnknownFormatNumber(anchorFormat)


  let d_mark_record offset_MarkArray classCount : mark_record decoder =
    let open DecodeOperation in
    d_uint16 >>= fun classId ->
    if classId > classCount then
      err @@ Error.InvalidMarkClass(classId)
    else
      d_fetch offset_MarkArray d_anchor >>= fun anchor ->
      return (classId, anchor)


  let d_mark_array classCount : (mark_record list) decoder =
    let open DecodeOperation in
    current >>= fun offset_MarkArray ->
    d_list (d_mark_record offset_MarkArray classCount)


  let d_base_record offset_BaseArray classCount : base_record decoder =
    let open DecodeOperation in
    d_repeat classCount (d_fetch_opt offset_BaseArray d_anchor) >>= fun anchors ->
    return (Array.of_list anchors)


  let d_base_array classCount : (base_record list) decoder =
    let open DecodeOperation in
    current >>= fun offset_BaseArray ->
    d_list (d_base_record offset_BaseArray classCount)


  let d_mark_to_base_attachment_subtable =
    let open DecodeOperation in
    current >>= fun offset_MarkBasePos ->
    d_uint16 >>= fun posFormat ->
    match posFormat with
    | 1 ->
        d_fetch offset_MarkBasePos d_coverage >>= fun markCoverage ->
        d_fetch offset_MarkBasePos d_coverage >>= fun baseCoverage ->
        d_uint16 >>= fun classCount ->
        d_fetch offset_MarkBasePos (d_mark_array classCount) >>= fun markArray ->
        d_fetch offset_MarkBasePos (d_base_array classCount) >>= fun baseArray ->
        combine_coverage markCoverage markArray >>= fun mark_assoc ->
        combine_coverage baseCoverage baseArray >>= fun base_assoc ->
        return (MarkBasePos1(classCount, mark_assoc, base_assoc))

    | _ ->
        err @@ Error.UnknownFormatNumber(posFormat)


  let d_component_record offset_LigatureAttach classCount : component_record decoder =
    let open DecodeOperation in
    d_repeat classCount (d_fetch_opt offset_LigatureAttach d_anchor) >>= fun anchoropts ->
    return (Array.of_list anchoropts)


  let d_ligature_attach classCount : ligature_attach decoder =
    let open DecodeOperation in
    current >>= fun offset_LigatureAttach ->
    d_list (d_component_record offset_LigatureAttach classCount)


  let d_ligature_array classCount : (ligature_attach list) decoder =
    let open DecodeOperation in
    current >>= fun offset_LigatureArray ->
    d_list (d_fetch offset_LigatureArray (d_ligature_attach classCount))


  let d_mark_to_ligature_attachment_subtable =
    let open DecodeOperation in
    current >>= fun offset_MarkLigPos ->
    d_uint16 >>= fun posFormat ->
    match posFormat with
    | 1 ->
        d_fetch offset_MarkLigPos d_coverage >>= fun markCoverage ->
        d_fetch offset_MarkLigPos d_coverage >>= fun ligatureCoverage ->
        d_uint16 >>= fun classCount ->
        d_fetch offset_MarkLigPos (d_mark_array classCount) >>= fun markArray ->
        d_fetch offset_MarkLigPos (d_ligature_array classCount) >>= fun ligatureArray ->
        combine_coverage markCoverage markArray >>= fun mark_assoc ->
        combine_coverage ligatureCoverage ligatureArray >>= fun ligature_assoc ->
        return (MarkLigPos1(classCount, mark_assoc, ligature_assoc))

    | _ ->
        err @@ Error.UnknownFormatNumber(posFormat)


  let d_mark2_record offset_Mark2Array classCount : mark2_record decoder =
    let open DecodeOperation in
    d_repeat classCount (d_fetch offset_Mark2Array d_anchor) >>= fun anchors ->
    return (Array.of_list anchors)


  let d_mark2_array  classCount : (mark2_record list) decoder =
    let open DecodeOperation in
    current >>= fun offset_Mark2Array ->
    d_list (d_mark2_record offset_Mark2Array classCount)


  let d_mark_to_mark_attachment_subtable =
    let open DecodeOperation in
    current >>= fun offset_MarkToMarkPos ->
    d_uint16 >>= fun posFormat ->
    match posFormat with
    | 1 ->
        d_fetch offset_MarkToMarkPos d_coverage >>= fun mark1Coverage ->
        d_fetch offset_MarkToMarkPos d_coverage >>= fun mark2Coverage ->
        d_uint16 >>= fun classCount ->
        d_fetch offset_MarkToMarkPos (d_mark_array classCount) >>= fun markArray ->
        d_fetch offset_MarkToMarkPos (d_mark2_array classCount) >>= fun mark2Array ->
        combine_coverage mark1Coverage markArray >>= fun mark_assoc ->
        combine_coverage mark2Coverage mark2Array >>= fun mark2_assoc ->
        return (MarkMarkPos1(classCount, mark_assoc, mark2_assoc))

    | _ ->
        err @@ Error.UnknownFormatNumber(posFormat)


  let lookup_exact offsets lookupType : (subtable list) decoder =
    let open DecodeOperation in
    match lookupType with
    | 1 ->
      (* Single adjustment positioning [page 192] *)
        pick_each offsets d_single_adjustment_subtable

    | 2 ->
      (* Pair adjustment positioning [page 194] *)
        pick_each offsets d_pair_adjustment_subtable

    | 3 ->
      (* Cursive attachment positioning [page 197] *)
        return []  (* TODO *)

    | 4 ->
      (* MarkToBase attachment positioning [page 198] *)
        pick_each offsets d_mark_to_base_attachment_subtable

    | 5 ->
      (* MarkToLigature attachment positioning [page 199] *)
        pick_each offsets d_mark_to_ligature_attachment_subtable

    | 6 ->
      (* MarkToMark attachment positioning [page 201] *)
        pick_each offsets d_mark_to_mark_attachment_subtable

    | 7 ->
      (* Contextual positioning [page 203] *)
        return []  (* TODO *)

    | 8 ->
      (* Chaining contextual positioning [page 209] *)
        return []  (* TODO *)

    | 9 ->
      (* Extension positioning [page 213] cannot occur here *)
        err Error.LayeredExtensionPosition

    | _ ->
        err @@ Error.UnknownGposLookupType(lookupType)


  let d_extension_position : (subtable list) decoder =
    let open DecodeOperation in
    (* The position is supposed to be set to the beginning of an ExtensionPosFormat1 subtable [page 213]. *)
    current >>= fun offset_ExtensionPos ->
    d_uint16 >>= fun posFormat ->
    match posFormat with
    | 1 ->
        d_uint16 >>= fun extensionLookupType ->
        d_long_offset offset_ExtensionPos >>= fun offset ->
        lookup_exact [offset] extensionLookupType

    | _ ->
        err @@ Error.UnknownFormatNumber(posFormat)


  let lookup : (subtable list) decoder =
    let open DecodeOperation in
    (* The position is supposed to be set to the beginning of a Lookup table [page 137]. *)
    current >>= fun offset_Lookup ->
    d_uint16 >>= fun lookupType ->
    d_uint16 >>= fun _lookupFlag ->
    d_list (d_offset offset_Lookup) >>= fun offsets ->
    match lookupType with
    | 9 ->
      (* Extension positioning [page 213] *)
        pick_each offsets d_extension_position >>= fun subtabless ->
        return (List.concat subtabless)

    | _ ->
        lookup_exact offsets lookupType


  type 'a folding_single1 = 'a -> glyph_id list -> value_record -> 'a

  type 'a folding_single2 = 'a -> glyph_id * value_record -> 'a

  type 'a folding_pair1 = 'a -> glyph_id * (glyph_id * value_record * value_record) list -> 'a

  type 'a folding_pair2 = class_definition list -> class_definition list -> 'a -> (class_value * (class_value * value_record * value_record) list) list -> 'a

  type 'a folding_markbase1 = int -> 'a -> (glyph_id * mark_record) list -> (glyph_id * base_record) list -> 'a

  type 'a folding_marklig1 = int -> 'a -> (glyph_id * mark_record) list -> (glyph_id * ligature_attach) list -> 'a

  type 'a folding_markmark1 = int -> 'a -> (glyph_id * mark_record) list -> (glyph_id * mark2_record) list -> 'a


  let fold_subtables
      ?single1:(f_single1 = (fun acc _ _ -> acc))
      ?single2:(f_single2 = (fun acc _ -> acc))
      ?pair1:(f_pair1 = (fun acc _ -> acc))
      ?pair2:(f_pair2 = (fun _ _ acc _ -> acc))
      ?markbase1:(f_markbase1 = (fun _ acc _ _ -> acc))
      ?marklig1:(f_marklig1 = (fun _ acc _ _ -> acc))
      ?markmark1:(f_markmark1 = (fun _ acc _ _ -> acc))
      (feature : feature) (acc : 'a) : 'a ok =
    let open ResultMonad in
    subtables_scheme lookup feature >>= fun subtabless ->
    let subtables = List.concat subtabless in
    let acc =
      subtables |> List.fold_left (fun acc subtable ->
        match subtable with
        | SinglePosAdjustment1(coverage, valrecs) ->
            f_single1 acc coverage valrecs

        | SinglePosAdjustment2(assoc) ->
            List.fold_left f_single2 acc assoc

        | PairPosAdjustment1(assoc) ->
            List.fold_left f_pair1 acc assoc

        | PairPosAdjustment2(clsdefs1, clsdefs2, assoc) ->
            f_pair2 clsdefs1 clsdefs2 acc assoc

        | MarkBasePos1(classCount, mark_assoc, base_assoc) ->
            f_markbase1 classCount acc mark_assoc base_assoc

        | MarkLigPos1(classCount, mark_assoc, ligature_assoc) ->
            f_marklig1 classCount acc mark_assoc ligature_assoc

        | MarkMarkPos1(classCount, mark_assoc, mark2_assoc) ->
            f_markmark1 classCount acc mark_assoc mark2_assoc
      ) acc
    in
    return acc

end

module Kern = struct

  include GeneralTable(struct type t = unit end)

  let make core ~offset ~length =
    make_scheme core offset length ()

end
