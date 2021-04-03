
open Basic
open Value
open DecodeBasic


type info = {
  num_glyphs    : int;
  num_h_metrics : int;
}

include GeneralTable(struct type t = info end)


let get (src : source) : t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Tag.table_hmtx >>= fun (offset, length) ->
  let num_glyphs = common.num_glyphs in
  let num_h_metrics = common.num_h_metrics in
  return @@ make_scheme common.core offset length { num_glyphs; num_h_metrics }


let access (hmtx : t) (gid : glyph_id) : ((int * int) option) ok =
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
