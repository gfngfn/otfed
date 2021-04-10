
open Basic
open DecodeBasic
open Value


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


let make_ttf_subset (ttf : ttf_source) (gids : glyph_id list) =
  let open ResultMonad in

  let src = Ttf(ttf) in

  let num_glyphs = List.length gids in

  (* Make `cmap`. *)
  inj_enc @@ EncodeTable.Cmap.make [] >>= fun _table_cmap ->
    (* TODO: support an option for embedding `cmap` tables *)

  (* Make `glyf` and `loca`. *)
  get_glyphs ttf gids >>= fun (gs, bbox_all) ->
  inj_enc @@ EncodeTable.Ttf.make_glyf gs >>= fun (_table_glyf, locs) ->
  inj_enc @@ EncodeTable.Ttf.make_loca locs >>= fun (_table_loca, index_to_loc_format) ->

  (* Make `hmtx` and get derived data for `hhea`. *)
  make_hmtx src (List.combine gids gs) >>= fun (_table_hmtx, hhea_derived, number_of_h_metrics) ->

  (* Make `hhea`. *)
  inj_dec @@ DecodeTable.Hhea.get src >>= fun { value = hhea_value; _ } ->
  let ihhea =
    Intermediate.Hhea.{
      value   = hhea_value;
      derived = hhea_derived;
    }
  in
  inj_enc @@ EncodeTable.Hhea.make ~number_of_h_metrics ihhea >>= fun _table_hhea ->

  (* Make `maxp` *)
  inj_dec @@ DecodeTable.Ttf.Maxp.get ttf >>= fun maxp ->
  let maxp =
    { maxp with num_glyphs = num_glyphs }  (* TODO: set more accurate data *)
  in
  inj_enc @@ EncodeTable.Ttf.Maxp.make maxp >>= fun _table_maxp ->

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
  inj_enc @@ EncodeTable.Head.make ihead >>= fun _table_head ->
  failwith "Encode.Subset.make_ttf"


let make_cff_subset (_cff : DecodeBasic.cff_source) (_gids : glyph_id list) =
  failwith "Encode.Subset.make_cff"


let make (src : DecodeBasic.source) (gids : glyph_id list) =
  match src with
  | Ttf(ttf) -> make_ttf_subset ttf gids
  | Cff(cff) -> make_cff_subset cff gids
