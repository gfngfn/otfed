
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


let d_long_hor_metric =
  let open DecodeOperation in
  d_uint16 >>= fun advanceWidth ->
  d_int16 >>= fun lsb ->
  return @@ (advanceWidth, lsb)


let d_left_side_bearing =
  let open DecodeOperation in
  d_int16


let access (hmtx : t) (gid : glyph_id) : ((design_units * design_units) option) ok =
  let open ResultMonad in
  let info = get_info hmtx in
  let num_h_metrics = info.num_h_metrics in
  if gid < 0 || info.num_glyphs <= gid then
    return None
  else if gid < num_h_metrics then
    let reloffset = 4 * gid in
    d_long_hor_metric |> DecodeOperation.run hmtx.core (hmtx.offset + reloffset) >>= fun (aw, lsb) ->
    return (Some((aw, lsb)))
  else
    let reloffset1 = 4 * (num_h_metrics - 1) in
    d_long_hor_metric |> DecodeOperation.run hmtx.core (hmtx.offset + reloffset1) >>= fun (aw_last, _) ->
    let reloffset2 = 4 * num_h_metrics + 2 * (gid - num_h_metrics) in
    d_left_side_bearing |> DecodeOperation.run hmtx.core (hmtx.offset + reloffset2) >>= fun lsb ->
    return (Some((aw_last, lsb)))
