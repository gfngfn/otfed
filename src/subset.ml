
open Basic
open Value


type error =
  | NoGlyphGiven
  | GlyphNotFound of glyph_id
  | DecodeError   of DecodeError.t
  | EncodeError   of EncodeError.t
[@@deriving show { with_path = false }]

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


let get_glyph (ttf : Decode.ttf_source) (gid : glyph_id) : ttf_glyph_info ok =
  let open ResultMonad in
  inj_dec @@ Decode.Ttf.loca ttf gid >>= function
  | None      -> err @@ GlyphNotFound(gid)
  | Some(loc) -> inj_dec @@ Decode.Ttf.glyf ttf loc


type glyph_accumulator = ttf_glyph_info Alist.t * bounding_box


let folding_glyph (ttf : Decode.ttf_source) ((gs, bbox_all) : glyph_accumulator) (gid : glyph_id) : glyph_accumulator ok =
  let open ResultMonad in
  get_glyph ttf gid >>= fun g ->
  return (Alist.extend gs g, update_bounding_box ~current:bbox_all ~new_one:(g.bounding_box))


let get_glyphs (ttf : Decode.ttf_source) (gids : glyph_id list) : (ttf_glyph_info list * bounding_box) ok =
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

type hmtx_accumulator = {
  current_hhea_derived : Intermediate.Hhea.derived;
  hmtx_entries         : hmtx_entry Alist.t;
  advance_width_sum    : wint;
  nonzero_width_count  : int;
}


let get_hmtx (ihmtx : Decode.Hmtx.t) ((gid, g) : glyph_id * ttf_glyph_info) : (Intermediate.Hhea.derived * hmtx_entry * design_units) ok =
  let open ResultMonad in
  inj_dec @@ Decode.Hmtx.access ihmtx gid >>= function
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
      return (derived, entry, aw)


let folding_hmtx (ihmtx : Decode.Hmtx.t) (acc : hmtx_accumulator) (gg : glyph_id * ttf_glyph_info) : hmtx_accumulator ok =
  let open ResultMonad in
  let derived = acc.current_hhea_derived in
  get_hmtx ihmtx gg >>= fun (derived_new, entry, aw) ->
      let derived =
        Intermediate.Hhea.{
          advance_width_max      = Stdlib.max derived.advance_width_max      derived_new.advance_width_max;
          min_left_side_bearing  = Stdlib.min derived.min_left_side_bearing  derived_new.min_left_side_bearing;
          min_right_side_bearing = Stdlib.min derived.min_right_side_bearing derived_new.min_right_side_bearing;
          xmax_extent            = Stdlib.max derived.xmax_extent            derived_new.xmax_extent;
        }
      in
      return {
        current_hhea_derived = derived;
        hmtx_entries         = Alist.extend acc.hmtx_entries entry;
        advance_width_sum    = acc.advance_width_sum +% !% aw;
        nonzero_width_count  = if aw = 0 then acc.nonzero_width_count else acc.nonzero_width_count + 1;
      }


type hmtx_result = {
  table_hmtx          : Encode.table;
  hhea_derived        : Intermediate.Hhea.derived;
  number_of_h_metrics : int;
  average_char_width  : design_units;
}


let make_hmtx (src : Decode.source) (ggs : (glyph_id * ttf_glyph_info) list) : hmtx_result ok =
  let open ResultMonad in
  inj_dec @@ Decode.Hmtx.get src >>= fun ihmtx ->
  match ggs with
  | [] ->
      err NoGlyphGiven

  | gg_first :: ggs_tail ->
      get_hmtx ihmtx gg_first >>= fun (derived_first, entry_first, aw) ->
      let acc =
        {
          current_hhea_derived = derived_first;
          hmtx_entries         = Alist.empty;
          advance_width_sum    = !% aw;
          nonzero_width_count  = if aw = 0 then 0 else 1;
        }
      in
      foldM (folding_hmtx ihmtx) ggs_tail acc >>= fun acc ->
      let hhea_derived = acc.current_hhea_derived in
      let entries_tail = acc.hmtx_entries in
      inj_enc @@ Encode.Hmtx.make (entry_first :: Alist.to_list entries_tail) >>= fun table_hmtx ->
      let number_of_h_metrics = List.length ggs in
      let average_char_width =
        let count = acc.nonzero_width_count in
        if count = 0 then 0 else WideInt.to_int (acc.advance_width_sum /% (!% count))
      in
      return {
        table_hmtx;
        hhea_derived;
        number_of_h_metrics;
        average_char_width;
      }


let make_os2 (src : Decode.source) ~average_char_width ~first_char ~max_context ~last_char : Encode.table ok =
  let open ResultMonad in
  inj_dec @@ Decode.Os2.get src >>= fun { value = os2_value; _ } ->
  let os2_derived =
    Intermediate.Os2.{
      x_avg_char_width    = average_char_width;
      us_first_char_index = first_char;
      us_last_char_index  = last_char;
      us_max_context      = max_context;
    }
  in
  let ios2 =
    Intermediate.Os2.{
      value   = os2_value;
      derived = os2_derived;
    }
  in
  inj_enc @@ Encode.Os2.make ios2


module OldToNewMap = Map.Make(Int)

type old_to_new_map = glyph_id OldToNewMap.t


let make_subset_of_subtable ~current_min:(uch_min : Uchar.t) ~current_max:(uch_max : Uchar.t) (old_to_new_map : old_to_new_map) (subtable_old : Value.Cmap.subtable) : Value.Cmap.subtable * Uchar.t * Uchar.t =
  let open Value.Cmap in
  let mapping_old = subtable_old.mapping in
  let (mapping_new, uch_min, uch_max) =
    Mapping.fold (fun uch gid_old ((mapping_new, uch_min, uch_max) as acc) ->
      match old_to_new_map |> OldToNewMap.find_opt gid_old with
      | None ->
          acc

      | Some(gid_new) ->
          let mapping_new = mapping_new |> Mapping.add_single uch gid_new in
          (mapping_new, Stdlib.min uch_min uch, Stdlib.max uch_max uch)
    ) mapping_old (Mapping.empty, uch_min, uch_max)
  in
  let subtable_new =
    {
      subtable_ids = subtable_old.subtable_ids;
      mapping      = mapping_new;
    }
  in
  (subtable_new, uch_min, uch_max)


let make_cmap (src : Decode.source) (gids : glyph_id list) : (Encode.table * Uchar.t * Uchar.t) ok =
  let open ResultMonad in
  let res_dec =
    Decode.Cmap.get src >>= fun icmap ->
    Decode.Cmap.get_subtables icmap >>= fun isubtables ->
    isubtables |> mapM Decode.Cmap.unmarshal_subtable
  in
  let (old_to_new_map, _) =
    gids |> List.fold_left (fun (old_to_new_map, gid_new) gid_old ->
      (old_to_new_map |> OldToNewMap.add gid_old gid_new, gid_new + 1)
    ) (OldToNewMap.empty, 0)
  in
  inj_dec res_dec >>= fun subtables ->
  let (subtable_new_acc, uch_min, uch_max) =
    subtables |> List.fold_left (fun (subtable_new_acc, uch_min, uch_max) subtable_old ->
      let (subtable_new, uch_min, uch_max) =
        make_subset_of_subtable
          ~current_min:uch_min
          ~current_max:uch_max
          old_to_new_map
          subtable_old
      in
      (Alist.extend subtable_new_acc subtable_new, uch_min, uch_max)
    ) (Alist.empty, Uchar.of_int 0xFFFF, Uchar.of_int 0)
  in
  let subtables_new = subtable_new_acc |> Alist.to_list in
  inj_enc @@ Encode.Cmap.make subtables_new >>= fun table_cmap ->
  return (table_cmap, uch_min, uch_max)


let make_ttf_subset ~(omit_cmap : bool) (ttf : Decode.ttf_source) (gids : glyph_id list) : string ok =
  let open ResultMonad in

  let src = Decode.Ttf(ttf) in

  let num_glyphs = List.length gids in

  (* Make `cmap`. *)
  make_cmap src gids >>= fun (table_cmap, first_char, last_char) ->

  (* Make `glyf` and `loca`. *)
  get_glyphs ttf gids >>= fun (gs, bbox_all) ->
  inj_enc @@ Encode.Ttf.make_glyf gs >>= fun (table_glyf, locs) ->
  inj_enc @@ Encode.Ttf.make_loca locs >>= fun (table_loca, index_to_loc_format) ->

  (* Make `post`. *)
  inj_dec @@ Decode.Post.get src >>= fun post ->
  inj_enc @@ Encode.Post.make post >>= fun table_post ->

  (* Make `name`. *)
  inj_dec @@ Decode.Name.get src >>= fun name ->
  inj_enc @@ Encode.Name.make name >>= fun table_name ->

  (* Make `hmtx` and get derived data for `hhea`. *)
  make_hmtx src (List.combine gids gs) >>= fun hmtx_result ->
  let { table_hmtx; hhea_derived; number_of_h_metrics; average_char_width; } = hmtx_result in

  (* Make `OS/2`. *)
  let max_context = Some(1) in (* TODO: should extend this if this function can encode advanced tables. *)
  make_os2 src ~average_char_width ~first_char ~last_char ~max_context >>= fun table_os2 ->

  (* Make `hhea`. *)
  inj_dec @@ Decode.Hhea.get src >>= fun { value = hhea_value; _ } ->
  let ihhea =
    Intermediate.Hhea.{
      value   = hhea_value;
      derived = hhea_derived;
    }
  in
  inj_enc @@ Encode.Hhea.make ~number_of_h_metrics ihhea >>= fun table_hhea ->

  (* Make `maxp` *)
  inj_dec @@ Decode.Ttf.Maxp.get ttf >>= fun maxp ->
  let maxp =
    { maxp with num_glyphs = num_glyphs }  (* TODO: set more accurate data *)
  in
  inj_enc @@ Encode.Ttf.Maxp.make maxp >>= fun table_maxp ->

  (* Make `head`. *)
  inj_dec @@ Decode.Head.get src >>= fun { value = head_value; _ } ->
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
  inj_enc @@ Encode.Head.make ihead >>= fun table_head ->
  inj_enc @@ Encode.make_font_data_from_tables @@ List.concat [
    [
      table_head;
      table_hhea;
      table_hmtx;
      table_maxp;
      table_post;
      table_name;
      table_os2;
      table_loca;
      table_glyf;
    ];
    if omit_cmap then [] else [table_cmap];
  ]


let make_cff_subset ~omit_cmap:(_ : bool) (_cff : Decode.cff_source) (_gids : glyph_id list) : string ok =
  failwith "Encode.Subset.make_cff"


let make ?(omit_cmap = false) (src : Decode.source) (gids : glyph_id list) : string ok =
  match src with
  | Ttf(ttf) -> make_ttf_subset ~omit_cmap ttf gids
  | Cff(cff) -> make_cff_subset ~omit_cmap cff gids
