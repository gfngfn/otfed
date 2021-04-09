
open Basic
open EncodeBasic


let make (imaxp : Intermediate.Ttf.Maxp.t) : table ok =
  let table_version = !%% 0x00010000L in
  let enc =
    let open EncodeOperation in
    e_uint32 table_version                  >>= fun () ->
    e_uint16 imaxp.num_glyphs               >>= fun () ->
    e_uint16 imaxp.max_points               >>= fun () ->
    e_uint16 imaxp.max_contours             >>= fun () ->
    e_uint16 imaxp.max_composite_points     >>= fun () ->
    e_uint16 imaxp.max_composite_contours   >>= fun () ->
    e_uint16 imaxp.max_zones                >>= fun () ->
    e_uint16 imaxp.max_twilight_points      >>= fun () ->
    e_uint16 imaxp.max_storage              >>= fun () ->
    e_uint16 imaxp.max_function_defs        >>= fun () ->
    e_uint16 imaxp.max_instruction_defs     >>= fun () ->
    e_uint16 imaxp.max_stack_elements       >>= fun () ->
    e_uint16 imaxp.max_size_of_instructions >>= fun () ->
    e_uint16 imaxp.max_component_elements   >>= fun () ->
    e_uint16 imaxp.max_component_depth      >>= fun () ->
    return ()
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_maxp;
    contents;
  }
