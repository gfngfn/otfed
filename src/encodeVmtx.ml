
open Basic
open Value
open EncodeBasic


let e_long_ver_metric (advanceHeight, topSideBearing) =
  let open EncodeOperation in
  e_uint16 advanceHeight >>= fun () ->
  e_int16 topSideBearing >>= fun () ->
  return ()


let e_top_side_bearing topSideBearing =
  let open EncodeOperation in
  e_int16 topSideBearing


let make_exact (long_ver_metrics : (design_units * design_units) list) (top_side_bearings : design_units list) : table ok =
  let enc =
    let open EncodeOperation in
    e_list e_long_ver_metric long_ver_metrics   >>= fun () ->
    e_list e_top_side_bearing top_side_bearings >>= fun () ->
    return ()
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_vmtx;
    contents;
  }


let make (metrics : (design_units * design_units) list) : table ok =
  make_exact metrics []
    (* TODO: optimize the resulting size by using leftSideBearing-only entries *)
