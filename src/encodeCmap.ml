
open Basic
open Value
open EncodeBasic
open EncodeOperation.Open


type header_entry = {
  offset_from_cmap : int;
  ids              : Value.Cmap.subtable_ids;
}

type incremental_state = {
  start_code_point : int;
  start_glyph_id   : glyph_id;
  prev_code_point  : int;
  prev_glyph_id    : glyph_id;
}

type temporary_segment = {
  segment_start    : int;
  segment_last     : int;
  segment_glyph_id : glyph_id;
}



let convert_to_temporary_segments (cmap_mapping : Value.Cmap.Mapping.t) : temporary_segment list =
  let (acc, state_opt) =
    Value.Cmap.Mapping.fold (fun uch gid (acc, state_opt) ->
      let cp = Uchar.to_int uch in
      match state_opt with
      | None ->
          let state =
            {
              start_code_point = cp;
              start_glyph_id   = gid;
              prev_code_point  = cp;
              prev_glyph_id    = gid;
            }
          in
          (acc, Some(state))

      | Some(state) ->
          if cp = state.prev_code_point + 1 && gid = state.prev_glyph_id + 1 then
          (* If the new entry can be appended to the segment built so far: *)
            let state =
              { state with
                prev_code_point = cp;
                prev_glyph_id   = gid;
              }
            in
            (acc, Some(state))
          else
            let temp_segment =
              {
                segment_start    = state.start_code_point;
                segment_last     = state.prev_code_point;
                segment_glyph_id = state.start_glyph_id
              }
            in
            let state =
              {
                start_code_point = cp;
                start_glyph_id   = gid;
                prev_code_point  = cp;
                prev_glyph_id    = gid;
              }
            in
            (Alist.extend acc temp_segment, Some(state))

    ) cmap_mapping (Alist.empty, None)
  in
  let acc =
    match state_opt with
    | None ->
        acc

    | Some(state) ->
        let temp_segment =
          {
            segment_start    = state.start_code_point;
            segment_last     = state.prev_code_point;
            segment_glyph_id = state.start_glyph_id
          }
        in
        Alist.extend acc temp_segment
  in
  Alist.to_list acc


let e_cmap_incrementals (temp_segments : temporary_segment list) : unit encoder =
  let open EncodeOperation in
  foldM (fun () temp_segment ->
    e_uint32 (!% (temp_segment.segment_start)) >>= fun () ->
    e_uint32 (!% (temp_segment.segment_last))  >>= fun () ->
    e_uint32 (!% (temp_segment.segment_glyph_id))
  ) temp_segments ()


let e_cmap_mapping (cmap_mapping : Value.Cmap.Mapping.t) : int encoder =
  let open EncodeOperation in
  let temp_segments = convert_to_temporary_segments cmap_mapping in
  let nGroups = List.length temp_segments in
  let language_id = !% 0 in (* TODO: language ID should be extracted from `cmap_subtable` *)
  let length = !% (16 + nGroups * 12) in
  current >>= fun reloffset ->
  e_uint16 12           >>= fun () -> (* Subtable format number 12. *)
  e_uint16 0            >>= fun () -> (* Reserved *)
  e_uint32 length       >>= fun () ->
  e_uint32 language_id  >>= fun () ->
  e_uint32 (!% nGroups) >>= fun () ->
  e_cmap_incrementals temp_segments >>= fun () ->
  return reloffset


let e_cmap_subtable ~(first_offset : int) (cmap_subtable : Value.Cmap.subtable) : header_entry encoder =
  let open EncodeOperation in
  e_cmap_mapping cmap_subtable.mapping >>= fun reloffset ->
  return {
    offset_from_cmap = first_offset + reloffset;
    ids              = cmap_subtable.subtable_ids;
  }


let make_subtables ~(first_offset : int) (cmap_subtables : Value.Cmap.subtable list) : (string * header_entry list) ok =
  let enc =
    let open EncodeOperation in
    mapM (e_cmap_subtable ~first_offset) cmap_subtables
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
