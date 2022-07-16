
open Basic
open DecodeBasic
open DecodeOperation.Open


let macintosh_glyph_name_array =
  Array.of_list macintosh_glyph_name_list


let d_pascal_string_names (len_remained : int) : (string array) decoder =
  let open DecodeOperation in
  let rec aux acc len_remained =
    if len_remained <= 0 then
      return @@ Array.of_list (Alist.to_list acc)
    else
      d_uint8 >>= fun len ->
      d_bytes len >>= fun name ->
      aux (Alist.extend acc name) (len_remained - 1 - len)
  in
  aux Alist.empty len_remained


let lookup_name (names : string array) (index : int) =
  let open DecodeOperation in
  try
    let name =
      if index < 258 then
        macintosh_glyph_name_array.(index)
      else
        names.(index - 258)
    in
    return name
  with
  | _ -> err @@ Error.InvalidGlyphNameIndex(index)



let d_version2 ~(num_glyphs : int) ~(remained_length : int) : Value.Post.GlyphNameArray.t decoder =
  let open DecodeOperation in
  d_skip 2 >>= fun () -> (* numberOfGlyphs *)
  d_repeat num_glyphs d_uint16 >>= fun indices ->
  d_pascal_string_names (remained_length - 2 - 2 * num_glyphs) >>= fun names ->
  indices |> mapM (lookup_name names) >>= fun gid_indexed_names ->
  return @@ Value.Post.GlyphNameArray.of_list gid_indexed_names


let d_post ~(num_glyphs : int) ~(length : int) =
  let open DecodeOperation in
  d_uint32 >>= fun version ->
  begin
    if version = !% 0x00010000 then
      return false
    else if version = !% 0x00020000 then
      return true
    else if version = !% 0x00030000 then
      return false
    else
      err @@ Error.UnknownTableVersion(version)
  end >>= fun is_long ->
  d_uint32 >>= fun italic_angle ->
  d_int16  >>= fun underline_position ->
  d_int16  >>= fun underline_thickness ->
  d_uint32 >>= fun is_fixed_pitch ->
  d_uint32 >>= fun min_mem_type_42 ->
  d_uint32 >>= fun max_mem_type_42 ->
  d_uint32 >>= fun min_mem_type_1 ->
  d_uint32 >>= fun max_mem_type_1 ->
  begin
    if is_long then
      d_version2 ~num_glyphs ~remained_length:(length - 32) >>= fun names ->
      return @@ Some(names)
    else
      return None
  end >>= fun names_opt ->
  return Value.Post.{
    value = {
      italic_angle = WideInt.to_int italic_angle;
      underline_position;
      underline_thickness;
      is_fixed_pitch  = (WideInt.is_pos is_fixed_pitch);
      min_mem_type_42 = WideInt.to_int min_mem_type_42;
      max_mem_type_42 = WideInt.to_int max_mem_type_42;
      min_mem_type_1  = WideInt.to_int min_mem_type_1;
      max_mem_type_1  = WideInt.to_int max_mem_type_1;
    };
    glyph_names = names_opt;
  }


let get (src : source) : Value.Post.t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_post >>= fun (offset, length) ->
  let num_glyphs = common.num_glyphs in
  d_post ~num_glyphs ~length |> DecodeOperation.run common.core offset
