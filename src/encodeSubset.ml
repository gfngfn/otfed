
open Basic
open Value
open DecodeBasic
open EncodeOperation.Open


type error =
  | NoGlyphGiven
  | GlyphNotFound of glyph_id
  | DecodeError   of DecodeError.t
  | EncodeError   of EncodeError.t

type 'a ok = ('a, error) result


let inj_dec d = Result.map_error (fun e -> DecodeError(e)) d
let inj_enc e = Result.map_error (fun e -> EncodeError(e)) e


let update_bounding_box ~current ~new_one =
  {
    x_min = Stdlib.min current.x_min new_one.x_min;
    y_min = Stdlib.min current.y_min new_one.y_min;
    x_max = Stdlib.max current.x_max new_one.x_max;
    y_max = Stdlib.max current.y_max new_one.y_max;
  }


let get_glyph (ttf : ttf_source) (gid : glyph_id) : ttf_glyph_info ok =
  let open ResultMonad in
  inj_dec @@ DecodeTable.Ttf.loca ttf gid >>= function
  | None      -> err @@ GlyphNotFound(gid)
  | Some(loc) -> inj_dec @@ DecodeTable.Ttf.glyf ttf loc


type glyph_accumulator = ttf_glyph_info Alist.t * bounding_box


let folding_glyph (ttf : ttf_source) ((gs, bbox_all) : glyph_accumulator) (gid : glyph_id) : glyph_accumulator ok =
  let open ResultMonad in
  get_glyph ttf gid >>= fun g ->
  return (Alist.extend gs g, update_bounding_box ~current:bbox_all ~new_one:(g.bounding_box))


let get_glyphs (ttf : ttf_source) (gids : glyph_id list) : (ttf_glyph_info list * bounding_box) ok =
  let open ResultMonad in
  match gids with
  | [] ->
      err NoGlyphGiven

  | gid_first :: gids_tail ->
      get_glyph ttf gid_first >>= fun g_first ->
      foldM (folding_glyph ttf) gids_tail (Alist.empty, g_first.bounding_box) >>= fun (gs_tail, bbox_all) ->
      let gs = g_first :: Alist.to_list gs_tail in
      return (gs, bbox_all)


type hmtx_entry = design_units * design_units

type hmtx_accumulator = Intermediate.Hhea.derived * (design_units * design_units) Alist.t


let get_hmtx (ihmtx : Decode.Hmtx.t) ((gid, g) : glyph_id * ttf_glyph_info) : (Intermediate.Hhea.derived * hmtx_entry) ok =
  let open ResultMonad in
  inj_dec @@ DecodeTable.Hmtx.access ihmtx gid >>= function
  | None ->
      err NoGlyphGiven

  | Some((aw, lsb) as entry) ->
      let derived =
        let x_min = g.bounding_box.x_min in
        let x_max = g.bounding_box.x_max in
        let extent = x_max - x_min in
        let rsb = aw - lsb - extent in
        Intermediate.Hhea.{
          advance_width_max      = aw;
          min_left_side_bearing  = lsb;
          min_right_side_bearing = rsb;
          xmax_extent            = extent;
        }
      in
      return (derived, entry)


let folding_hmtx (ihmtx : Decode.Hmtx.t) ((derived, entries) : hmtx_accumulator) (gg : glyph_id * ttf_glyph_info) =
  let open ResultMonad in
  get_hmtx ihmtx gg >>= fun (derived_new, entry) ->
      let derived =
        Intermediate.Hhea.{
          advance_width_max      = Stdlib.max derived.advance_width_max      derived_new.advance_width_max;
          min_left_side_bearing  = Stdlib.min derived.min_left_side_bearing  derived_new.min_left_side_bearing;
          min_right_side_bearing = Stdlib.min derived.min_right_side_bearing derived_new.min_right_side_bearing;
          xmax_extent            = Stdlib.max derived.xmax_extent            derived_new.xmax_extent;
        }
      in
      return (derived, Alist.extend entries entry)


let make_hmtx (src : source) (ggs : (glyph_id * ttf_glyph_info) list) : (EncodeBasic.table * Intermediate.Hhea.derived * int) ok =
  let open ResultMonad in
  inj_dec @@ DecodeTable.Hmtx.get src >>= fun ihmtx ->
  match ggs with
  | [] ->
      err NoGlyphGiven

  | gg_first :: ggs_tail ->
      get_hmtx ihmtx gg_first >>= fun (derived_first, entry_first) ->
      foldM (folding_hmtx ihmtx) ggs_tail (derived_first, Alist.empty) >>= fun (derived, entries_tail) ->
      inj_enc @@ EncodeTable.Hmtx.make (entry_first :: Alist.to_list entries_tail) >>= fun table_hmtx ->
      let number_of_h_metrics = List.length ggs in
      return (table_hmtx, derived, number_of_h_metrics)


type relative_offset = int

type table_directory_entry = Tag.t * relative_offset * wint

type table_accumulator = relative_offset option * table_directory_entry Alist.t


let add_checksum (x : wint) (y : wint) : wint =
  let open WideInt in
    let q = (of_int 1) lsl 32 in
    (x +% y) mod q


let calculate_checksum (s : string) : wint =
  let open WideInt in
  let len = String.length s in
  let access i =
    if i < len then
      of_byte (String.get s i)
    else
      !% 0
  in
  let rec aux acc i =
    if i >= len then
      acc
    else
      let b0 = access i in
      let b1 = access (i + 1) in
      let b2 = access (i + 2) in
      let b3 = access (i + 3) in
      let ui = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
      let acc = add_checksum acc ui in
      aux acc (i + 4)
  in
  aux (of_int 0) 0


let e_single_table ((checksum_reloffset_opt, entries) : table_accumulator) (table : EncodeBasic.table) =
  let open EncodeOperation in
  pad_to_long_aligned    >>= fun () ->
  current                >>= fun reloffset ->
  e_bytes table.contents >>= fun () ->
  let checksum_reloffset_opt =
    if Tag.equal Tag.table_head table.tag then
      Some(reloffset + 8)
    else
      checksum_reloffset_opt
  in
  let table_checksum = calculate_checksum table.contents in
  let entry = (table.tag, reloffset, table_checksum) in
  return (checksum_reloffset_opt, Alist.extend entries entry)


let enc_tables (tables : EncodeBasic.table list) : (relative_offset * table_directory_entry list) encoder =
  let open EncodeOperation in
  foldM e_single_table tables (None, Alist.empty) >>= fun (checksum_reloffset_opt, entries) ->
  match checksum_reloffset_opt with
  | None                     -> assert false
  | Some(checksum_reloffset) -> return (checksum_reloffset, Alist.to_list entries)


let enc_table_directory_entry ~first_offset (all_table_checksum : wint) ((tag, reloffset, checksum) : table_directory_entry) : wint encoder =
  let open EncodeOperation in
  e_tag tag                                >>= fun () ->
  e_uint32 checksum                        >>= fun () ->
  e_uint32 (!% (first_offset + reloffset)) >>= fun () ->
  return @@ add_checksum all_table_checksum checksum


let enc_table_directory_entries ~first_offset (entries : table_directory_entry list) : wint encoder =
  let open EncodeOperation in
  foldM (enc_table_directory_entry ~first_offset) entries (!% 0)


let cut_uint32_to_bytes (u : wint) : char * char * char * char =
  let open WideInt in
  let b0 = u lsr 24 in
  let r0 = u -% (b0 lsl 24) in
  let b1 = r0 lsr 16 in
  let r1 = r0 -% (b1 lsl 16) in
  let b2 = r1 lsr 8 in
  let b3 = r1 -% (b2 lsl 8) in
  (to_byte b0, to_byte b1, to_byte b2, to_byte b3)


let update_checksum_adjustment ~checksum_offset ~checksum_value (contents : string) =
  let checksum_adjustment =
    let temp = (!%% 0xB1B0AFBAL) -% checksum_value in
    if WideInt.is_neg temp then temp +% (!% (1 lsl 32)) else temp
  in
  try
    let bytes = Bytes.of_string contents in
    let (b0, b1, b2, b3) = cut_uint32_to_bytes checksum_adjustment in
    Bytes.set bytes checksum_offset       b0;
    Bytes.set bytes (checksum_offset + 1) b1;
    Bytes.set bytes (checksum_offset + 2) b2;
    Bytes.set bytes (checksum_offset + 3) b3;
    Bytes.to_string bytes
  with
  | _ -> assert false


let enc_header (numTables : int) =
  let open EncodeOperation in
  let entrySelector = Stdlib.(truncate (log (float_of_int numTables) /. log 2.0)) in
  let searchRange = 1 lsl entrySelector in
  let rangeShift = numTables * 16 - searchRange in
  e_uint32 (!% 0x00010000) >>= fun () ->
  e_uint16 numTables       >>= fun () ->
  e_uint16 searchRange     >>= fun () ->
  e_uint16 entrySelector   >>= fun () ->
  e_uint16 rangeShift      >>= fun () ->
  return ()


let make_font_data_from_tables (tables : EncodeBasic.table list) : string ok =
  let tables = tables |> List.sort EncodeBasic.compare_table in
  let numTables = List.length tables in
  let first_offset = 12 + numTables * 12 in
  let open ResultMonad in
  inj_enc (enc_tables tables |> EncodeOperation.run) >>= fun (table_contents, (checksum_reloffset, entries)) ->
  let enc =
    let open EncodeOperation in
    enc_header numTables                              >>= fun () ->
    enc_table_directory_entries ~first_offset entries >>= fun all_table_checksum ->
    e_bytes table_contents                            >>= fun () ->
    return all_table_checksum
  in
  inj_enc (enc |> EncodeOperation.run) >>= fun (contents, all_table_checksum) ->
  let checksum_offset = first_offset + checksum_reloffset in
  let prelude_checksum = calculate_checksum (String.sub contents 0 first_offset) in
  let checksum_value = add_checksum prelude_checksum all_table_checksum in
  return (update_checksum_adjustment ~checksum_offset ~checksum_value contents)


let make_ttf_subset (ttf : ttf_source) (gids : glyph_id list) =
  let open ResultMonad in

  let src = Ttf(ttf) in

  let num_glyphs = List.length gids in

  (* Make `cmap`. *)
  inj_enc @@ EncodeTable.Cmap.make [] >>= fun table_cmap ->
    (* TODO: support an option for embedding `cmap` tables *)

  (* Make `glyf` and `loca`. *)
  get_glyphs ttf gids >>= fun (gs, bbox_all) ->
  inj_enc @@ EncodeTable.Ttf.make_glyf gs >>= fun (table_glyf, locs) ->
  inj_enc @@ EncodeTable.Ttf.make_loca locs >>= fun (table_loca, index_to_loc_format) ->

  (* Make `hmtx` and get derived data for `hhea`. *)
  make_hmtx src (List.combine gids gs) >>= fun (table_hmtx, hhea_derived, number_of_h_metrics) ->

  (* Make `hhea`. *)
  inj_dec @@ DecodeTable.Hhea.get src >>= fun { value = hhea_value; _ } ->
  let ihhea =
    Intermediate.Hhea.{
      value   = hhea_value;
      derived = hhea_derived;
    }
  in
  inj_enc @@ EncodeTable.Hhea.make ~number_of_h_metrics ihhea >>= fun table_hhea ->

  (* Make `maxp` *)
  inj_dec @@ DecodeTable.Ttf.Maxp.get ttf >>= fun maxp ->
  let maxp =
    { maxp with num_glyphs = num_glyphs }  (* TODO: set more accurate data *)
  in
  inj_enc @@ EncodeTable.Ttf.Maxp.make maxp >>= fun table_maxp ->

  (* Make `head`. *)
  inj_dec @@ DecodeTable.Head.get src >>= fun { value = head_value; _ } ->
  let head_derived =
    Intermediate.Head.{
      x_min = bbox_all.x_min;
      y_min = bbox_all.y_min;
      x_max = bbox_all.x_max;
      y_max = bbox_all.y_max;
      index_to_loc_format;
    }
  in
  let ihead =
    Intermediate.Head.{
      value   = head_value;
      derived = head_derived;
    }
  in
  inj_enc @@ EncodeTable.Head.make ihead >>= fun table_head ->
  make_font_data_from_tables [
    table_head;
    table_hhea;
    table_hmtx;
    table_maxp;
    table_cmap;
    table_loca;
    table_glyf;
  ]


let make_cff_subset (_cff : DecodeBasic.cff_source) (_gids : glyph_id list) =
  failwith "Encode.Subset.make_cff"


let make (src : DecodeBasic.source) (gids : glyph_id list) =
  match src with
  | Ttf(ttf) -> make_ttf_subset ttf gids
  | Cff(cff) -> make_cff_subset cff gids
