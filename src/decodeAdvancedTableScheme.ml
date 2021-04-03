
open Basic
open DecodeBasic
open DecodeOperation.Open


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
