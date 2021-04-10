
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


let get_glyph (ttf : ttf_source) (gid : glyph_id) =
  let open ResultMonad in
  inj_dec @@ DecodeTable.Ttf.loca ttf gid >>= function
  | None      -> err @@ GlyphNotFound(gid)
  | Some(loc) -> inj_dec @@ DecodeTable.Ttf.glyf ttf loc


type glyph_accumulator = ttf_glyph_description Alist.t * bounding_box


let folding_glyph (ttf : ttf_source) ((descrs, bbox_all) : glyph_accumulator) (gid : glyph_id) : glyph_accumulator ok =
  let open ResultMonad in
  get_glyph ttf gid >>= fun (descr, bbox) ->
  return (Alist.extend descrs descr, update_bounding_box ~current:bbox_all ~new_one:bbox)


let get_glyphs (ttf : ttf_source) (gids : glyph_id list) : (ttf_glyph_description list * bounding_box) ok =
  let open ResultMonad in
  match gids with
  | [] ->
      err NoGlyphGiven

  | gid_first :: gids_tail ->
      get_glyph ttf gid_first >>= fun (descr_first, bbox_first) ->
      foldM (folding_glyph ttf) gids_tail (Alist.empty, bbox_first) >>= fun (descrs_tail, bbox_all) ->
      let descrs = descr_first :: Alist.to_list descrs_tail in
      return (descrs, bbox_all)


let make_ttf_subset (ttf : ttf_source) (gids : glyph_id list) =
  let open ResultMonad in
  get_glyphs ttf gids >>= fun (_descrs, bbox_all) ->
  inj_dec @@ DecodeTable.Head.get (Ttf(ttf)) >>= fun { value = head_value; _ } ->
  let head_derived =
    Intermediate.Head.{
      x_min               = bbox_all.x_min;
      y_min               = bbox_all.y_min;
      x_max               = bbox_all.x_max;
      y_max               = bbox_all.y_max;
      index_to_loc_format = failwith "TODO: head_derived, loc_format";
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
