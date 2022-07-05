
open Basic
open Value
open EncodeBasic


let e_long_hor_metric (advanceWidth, lsb) =
  let open EncodeOperation in
  e_uint16 advanceWidth >>= fun () ->
  e_int16  lsb          >>= fun () ->
  return ()


let e_left_side_bearing lsb =
  let open EncodeOperation in
  e_int16 lsb


let make_exact (long_hor_metrics : (design_units * design_units) list) (left_side_bearings : design_units list) : table ok =
  let enc =
    let open EncodeOperation in
    e_list e_long_hor_metric long_hor_metrics     >>= fun () ->
    e_list e_left_side_bearing left_side_bearings >>= fun () ->
    return ()
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_hmtx;
    contents;
  }


let make (metrics : (design_units * design_units) list) : table ok =
  make_exact metrics []
    (* TODO: optimize the resulting size by using leftSideBearing-only entries *)
