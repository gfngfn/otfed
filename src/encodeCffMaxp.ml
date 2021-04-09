
open Basic
open EncodeBasic


let make (imaxp : Intermediate.Cff.Maxp.t) : table ok =
  let table_version = !%% 0x00005000L in
  let enc =
    let open EncodeOperation in
    e_uint32 table_version    >>= fun () ->
    e_uint16 imaxp.num_glyphs >>= fun () ->
    return ()
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_maxp;
    contents;
  }
