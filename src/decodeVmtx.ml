
open Basic
open Value
open DecodeBasic


type info = {
  num_glyphs           : int;
  num_long_ver_metrics : int;
}

include GeneralTable(struct type t = info end)


let get (src : source) : (t option) ok =
  let open ResultMonad in
  let common = get_common_source src in
  match DecodeOperation.seek_table common.table_directory Tag.table_hmtx with
  | None ->
      return None

  | Some((offset, length)) ->
      let num_glyphs = common.num_glyphs in
      DecodeVhea.fetch_num_of_long_ver_metrics common.core common.table_directory >>= function
      | None ->
          err Error.NoVerticalHeader

      | Some(num_long_ver_metrics) ->
          let info = { num_glyphs; num_long_ver_metrics } in
          return @@ Some(make_scheme common.core offset length info)


let d_long_ver_metric =
  let open DecodeOperation in
  d_uint16 >>= fun advanceHeight ->
  d_int16 >>= fun topSideBearing ->
  return @@ (advanceHeight, topSideBearing)


let d_top_side_bearing =
  let open DecodeOperation in
  d_int16


let access (vmtx : t) (gid : glyph_id) : ((design_units * design_units) option) ok =
  let open ResultMonad in
  let { num_glyphs; num_long_ver_metrics } = get_info vmtx in
  if gid < 0 || num_glyphs <= gid then
    return None
  else if gid < num_long_ver_metrics then
    let reloffset = 4 * gid in
    d_long_ver_metric |> DecodeOperation.run vmtx.core (vmtx.offset + reloffset) >>= fun (ah, tsb) ->
    return (Some((ah, tsb)))
  else
    let reloffset1 = 4 * (num_long_ver_metrics - 1) in
    d_long_ver_metric |> DecodeOperation.run vmtx.core (vmtx.offset + reloffset1) >>= fun (ah_last, _) ->
    let reloffset2 = 4 * num_long_ver_metrics + 2 * (gid - num_long_ver_metrics) in
    d_top_side_bearing |> DecodeOperation.run vmtx.core (vmtx.offset + reloffset2) >>= fun tsb ->
    return (Some((ah_last, tsb)))
