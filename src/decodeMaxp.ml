
open Basic
open DecodeBasic


type t = {
  num_glyphs               : int;
  max_points               : int;
  max_contours             : int;
  max_composite_points     : int;
  max_composite_contours   : int;
  max_zones                : int;
  max_twilight_points      : int;
  max_storage              : int;
  max_function_defs        : int;
  max_instruction_defs     : int;
  max_stack_elements       : int;
  max_size_of_instructions : int;
  max_component_elements   : int;
  max_component_depth      : int;
}
[@@deriving show { with_path = false }]


let get (src : source) : t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_maxp >>= fun (offset, _length) ->
  let dec =
    let open DecodeOperation in
    d_uint32 >>= fun version ->
    if version = !%% 0x00005000L then
    (* If the font is based on CFF *)
      d_uint16 >>= fun num_glyphs ->
      return {
        num_glyphs               = num_glyphs;
        max_points               = 0;
        max_contours             = 0;
        max_composite_points     = 0;
        max_composite_contours   = 0;
        max_zones                = 0;
        max_twilight_points      = 0;
        max_storage              = 0;
        max_function_defs        = 0;
        max_instruction_defs     = 0;
        max_stack_elements       = 0;
        max_size_of_instructions = 0;
        max_component_elements   = 0;
        max_component_depth      = 0;
      }
    else if version = !%% 0x00010000L then
    (* If the font is based on TTF *)
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
      return {
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
  in
  DecodeOperation.run common.core offset dec
