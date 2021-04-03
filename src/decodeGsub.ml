
open Basic
open Value
open DecodeBasic
open DecodeOperation.Open


include DecodeAdvancedTableScheme


let get (src : source) : (t option) ok =
  let open ResultMonad in
  let common = get_common_source src in
  match DecodeOperation.seek_table common.table_directory Tag.table_gsub with
  | None ->
      return None

  | Some((offset, length)) ->
      return @@ Some(make common.core ~offset ~length)


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
