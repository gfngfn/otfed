
open Basic
open Value
open EncodeBasic


let make (entries : (int * int) list) : table ok =
  let enc =
    let open EncodeOperation in
    let e_entry (advanceWidth, lsb) =
      e_uint16 advanceWidth >>= fun () ->
      e_int16  lsb
    in
    e_list e_entry entries
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_hmtx;
    contents;
  }


let make_by_iter ~num_glyphs:(num_glyphs : int) (f : glyph_id -> int * int) : table ok =
  let enc =
    let open EncodeOperation in
    let rec aux gid =
      if gid >= num_glyphs then
        return ()
      else
        let (advanceWidth, lsb) = f gid in
        e_uint16 advanceWidth >>= fun () ->
        e_int16  lsb          >>= fun () ->
        aux (gid + 1)
    in
    aux 0
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_hmtx;
    contents;
  }
