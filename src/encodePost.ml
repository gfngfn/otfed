
open Basic
open EncodeBasic


(* TODO: writes PostScript names *)
let make (post : Value.Post.t) =
  let v = post.value in
  let table_version = 0x00030000 in
  let is_fixed_pitch_num = if v.is_fixed_pitch then 1 else 0 in
  let enc =
    let open EncodeOperation in
    e_uint32 (!% table_version)        >>= fun () ->
    e_uint32 (!% (v.italic_angle))     >>= fun () ->
    e_uint16 v.underline_position      >>= fun () ->
    e_uint16 v.underline_thickness     >>= fun () ->
    e_uint32 (!% (is_fixed_pitch_num)) >>= fun () ->
    e_uint32 (!% (v.min_mem_type_42))  >>= fun () ->
    e_uint32 (!% (v.max_mem_type_42))  >>= fun () ->
    e_uint32 (!% (v.min_mem_type_1))   >>= fun () ->
    e_uint32 (!% (v.max_mem_type_1))   >>= fun () ->
    return ()
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_post;
    contents;
  }
