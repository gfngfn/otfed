
open Basic
open DecodeBasic


let d_maxp =
  let open DecodeOperation in
  d_uint32 >>= fun version ->
  if version = !%% 0x00005000L then
    err @@ Error.TtfContainsCffMaxpTable
  else if version = !%% 0x00010000L then
    d_uint16 >>= fun num_glyphs ->
    d_uint16 >>= fun max_points ->
    d_uint16 >>= fun max_contours ->
    d_uint16 >>= fun max_composite_points ->
    d_uint16 >>= fun max_composite_contours ->
    d_uint16 >>= fun max_zones ->
    d_uint16 >>= fun max_twilight_points ->
    d_uint16 >>= fun max_storage ->
    d_uint16 >>= fun max_function_defs ->
    d_uint16 >>= fun max_instruction_defs ->
    d_uint16 >>= fun max_stack_elements ->
    d_uint16 >>= fun max_size_of_instructions ->
    d_uint16 >>= fun max_component_elements ->
    d_uint16 >>= fun max_component_depth ->
    return Intermediate.Ttf.Maxp.{
      num_glyphs;
      max_points;
      max_contours;
      max_composite_points;
      max_composite_contours;
      max_zones;
      max_twilight_points;
      max_storage;
      max_function_defs;
      max_instruction_defs;
      max_stack_elements;
      max_size_of_instructions;
      max_component_elements;
      max_component_depth;
    }
  else
    err @@ Error.UnknownTableVersion(version)


let get (ttf : ttf_source) : Intermediate.Ttf.Maxp.t ok =
  let open ResultMonad in
  let common = ttf.ttf_common in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_maxp >>= fun (offset, _length) ->
  DecodeOperation.run common.core offset d_maxp
