
open Basic
open EncodeBasic


let e_post (post : Value.Post.t) =
  let open EncodeOperation in
  let v = post.value in
  let table_version = 0x00030000 in
  let is_fixed_pitch_num = if v.is_fixed_pitch then 1 else 0 in
  e_uint32 (!% table_version)        >>= fun () ->
  e_uint32 (!% (v.italic_angle))     >>= fun () ->
  e_int16  v.underline_position      >>= fun () ->
  e_int16  v.underline_thickness     >>= fun () ->
  e_uint32 (!% (is_fixed_pitch_num)) >>= fun () ->
  e_uint32 (!% (v.min_mem_type_42))  >>= fun () ->
  e_uint32 (!% (v.max_mem_type_42))  >>= fun () ->
  e_uint32 (!% (v.min_mem_type_1))   >>= fun () ->
  e_uint32 (!% (v.max_mem_type_1))   >>= fun () ->
  return ()


(* TODO: writes PostScript names *)
let make (post : Value.Post.t) =
  let open ResultMonad in
  e_post post |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_post;
    contents;
  }
