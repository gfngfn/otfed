
open Basic
open EncodeBasic
open EncodeOperation.Open


type header_entry = {
  offset_from_cmap : int;
  ids              : Value.Cmap.subtable_ids;
}


let make_subtables ~first_offset:(first_offset : int) (cmap_subtables : Value.Cmap.subtable list) : (string * header_entry list) ok =
  let e_single (cmap_subtable : Value.Cmap.subtable) =
    let nGroups = Value.Cmap.Mapping.number_of_entries cmap_subtable.mapping in
    let language_id = !% 0 in (* TODO: language ID should be extracted from `cmap_subtable` *)
    let length = !% (16 + nGroups * 12) in
    let open EncodeOperation in
    current >>= fun reloffset ->
    e_uint16 12          >>= fun () -> (* Subtable format number 12. *)
    e_uint16 0           >>= fun () -> (* Reserved *)
    e_uint32 language_id >>= fun () ->
    e_uint32 length      >>= fun () ->
    Value.Cmap.Mapping.fold (fun uch gid enc ->
      let cp = !% (Uchar.to_int uch) in
      enc >>= fun () ->
      e_uint32 cp >>= fun () ->
      e_uint32 cp >>= fun () ->
      e_uint32 (!% gid)
    ) cmap_subtable.mapping (return ()) >>= fun () ->
    return {
      offset_from_cmap = first_offset + reloffset;
      ids              = cmap_subtable.subtable_ids;
    }
  in
  let enc =
    let open EncodeOperation in
    mapM e_single cmap_subtables
  in
  enc |> EncodeOperation.run


let e_header_entry (entry : header_entry) : unit encoder =
  let open EncodeOperation in
  e_uint16 entry.ids.platform_id >>= fun () ->
  e_uint16 entry.ids.encoding_id >>= fun () ->
  e_uint32 (!% (entry.offset_from_cmap))


let make (cmap : Value.Cmap.t) =
  let cmap_subtables = Value.Cmap.subtables cmap in
  let cmap_subtables = cmap_subtables |> List.sort Value.Cmap.compare_subtables in
  let numTables = List.length cmap_subtables in
  let table_version = 0 in
  let first_offset = 4 + numTables * 8 in (* the length of the `cmap` header and encoding records. *)
  let open ResultMonad in
  make_subtables ~first_offset cmap_subtables >>= fun (subtable_contents, header_entries) ->
  let enc =
    let open EncodeOperation in
    e_uint16 table_version               >>= fun () ->
    e_uint16 numTables                   >>= fun () ->
    e_list e_header_entry header_entries >>= fun () ->
    e_bytes subtable_contents
  in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_cmap;
    contents;
  }
