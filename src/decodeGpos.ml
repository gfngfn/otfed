
open Basic
open Value
open DecodeBasic
open DecodeOperation.Open


include DecodeAdvancedTableScheme


let get (src : source) : (t option) ok =
  let open ResultMonad in
  let common = get_common_source src in
  match DecodeOperation.seek_table common.table_directory Tag.table_gpos with
  | None ->
      return None

  | Some((offset, length)) ->
      return @@ Some(make common.core ~offset ~length)


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
      d_device >>= fun xdevtable ->
      d_device >>= fun ydevtable ->
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
