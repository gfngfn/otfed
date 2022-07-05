
open Basic
open EncodeBasic


let e_maxp (imaxp : Intermediate.Cff.Maxp.t) =
  let open EncodeOperation in
  let table_version = !%% 0x00005000L in
  e_uint32 table_version    >>= fun () ->
  e_uint16 imaxp.num_glyphs >>= fun () ->
  return ()


let make (imaxp : Intermediate.Cff.Maxp.t) : table ok =
  let open ResultMonad in
  e_maxp imaxp |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_maxp;
    contents;
  }
