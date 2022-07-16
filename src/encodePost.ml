
open Basic
open EncodeBasic


module NameToBiasedIndexMap = Map.Make(String)


let macintosh_glyph_name_map =
  let (_, map) =
    macintosh_glyph_name_list |> List.fold_left (fun (index, map) name ->
      (index + 1, map |> NameToBiasedIndexMap.add name index)
    ) (0, NameToBiasedIndexMap.empty)
  in
  map


let e_common (v : Value.Post.value) =
  let open EncodeOperation in
  let is_fixed_pitch_num = if v.is_fixed_pitch then 1 else 0 in
  e_uint32 (!% (v.italic_angle))     >>= fun () ->
  e_int16  v.underline_position      >>= fun () ->
  e_int16  v.underline_thickness     >>= fun () ->
  e_uint32 (!% (is_fixed_pitch_num)) >>= fun () ->
  e_uint32 (!% (v.min_mem_type_42))  >>= fun () ->
  e_uint32 (!% (v.max_mem_type_42))  >>= fun () ->
  e_uint32 (!% (v.min_mem_type_1))   >>= fun () ->
  e_uint32 (!% (v.max_mem_type_1))   >>= fun () ->
  return ()


let e_post3 (v : Value.Post.value) =
  let open EncodeOperation in
  let table_version = 0x00030000 in
  e_uint32 (!% table_version) >>= fun () ->
  e_common v


type indexing_state = {
  next_index        : int;
  name_to_index     : int NameToBiasedIndexMap.t;
  index_accumulator : int Alist.t;
  name_accumulator  : string Alist.t;
}


let make_index_list_and_name_list (glyph_names : string list) =
  let state =
    glyph_names |> List.fold_left (fun state name ->
      match state.name_to_index |> NameToBiasedIndexMap.find_opt name with
      | Some(i) ->
          { state with
            index_accumulator = Alist.extend state.index_accumulator i;
          }

      | None ->
          let i = state.next_index in
          {
            next_index        = i + 1;
            name_to_index     = state.name_to_index |> NameToBiasedIndexMap.add name i;
            index_accumulator = Alist.extend state.index_accumulator i;
            name_accumulator  = Alist.extend state.name_accumulator name;
          }

    ) {
      next_index        = 258;
      name_to_index     = macintosh_glyph_name_map;
      index_accumulator = Alist.empty;
      name_accumulator  = Alist.empty;
    }
  in
  (Alist.to_list state.index_accumulator, Alist.to_list state.name_accumulator)


let e_pascal_string (s : string) =
  let open EncodeOperation in
  e_uint8 (String.length s) >>= fun () ->
  e_bytes s


let e_post2 ~(num_glyphs : int) (v : Value.Post.value) (glyph_names : Value.Post.GlyphNameArray.t) =
  let open EncodeOperation in
  let len = Value.Post.GlyphNameArray.length glyph_names in
  if len <> num_glyphs then
    err @@ InvalidNumberOfGlyphNames {
      expected = num_glyphs;
      got      = len;
    }
  else
    let (indices, names) = make_index_list_and_name_list (Value.Post.GlyphNameArray.to_list glyph_names) in
    let table_version = 0x00020000 in
    e_uint32 (!% table_version) >>= fun () ->
    e_common v                  >>= fun () ->
    e_uint16 num_glyphs         >>= fun () ->
    foldM (fun () index -> e_uint16 index) indices () >>= fun () ->
    foldM (fun () name -> e_pascal_string name) names () >>= fun () ->
    return ()


let e_post ~(num_glyphs : int) (post : Value.Post.t) =
  let v = post.value in
  match post.glyph_names with
  | None              -> e_post3 v
  | Some(glyph_names) -> e_post2 ~num_glyphs v glyph_names


let make ~(num_glyphs : int) (post : Value.Post.t) =
  let open ResultMonad in
  e_post ~num_glyphs post |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_post;
    contents;
  }
