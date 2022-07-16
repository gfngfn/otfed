
open Basic
open DecodeBasic
open DecodeOperation.Open


(* Reads a NameRecord [page 26]. *)
let d_name_record ~offset_string_storage:(offset_string_storage : offset) =
  let open DecodeOperation in
  d_uint16 >>= fun platform_id ->
  d_uint16 >>= fun encoding_id ->
  d_uint16 >>= fun language_id ->
  d_uint16 >>= fun name_id ->
  d_uint16 >>= fun length ->
  d_fetch offset_string_storage (d_bytes length) >>= fun name ->
  return Value.Name.{
    platform_id;
    encoding_id;
    language_id;
    name_id;
    name;
  }


let d_name_records ~count ~offset_string_storage:(offset_string_storage : offset) : (Value.Name.name_record list) decoder =
  let open DecodeOperation in
  d_repeat count (d_name_record ~offset_string_storage)


let d_lang_tag_record ~offset_string_storage:(offset_string_storage : offset) : Value.Name.lang_tag decoder =
  let open DecodeOperation in
  d_uint16 >>= fun length ->
  d_fetch offset_string_storage (d_bytes length)


let d_lang_tag_records ~offset_string_storage:(offset_string_storage : offset) : (Value.Name.lang_tag list) decoder =
  let open DecodeOperation in
  d_uint16 >>= fun lang_tag_count ->
  d_repeat lang_tag_count (d_lang_tag_record ~offset_string_storage)


let d_name =
  let open DecodeOperation in
  current >>= fun offset_name ->
  d_uint16 >>= fun format ->
  d_uint16 >>= fun count ->
  d_offset offset_name >>= fun offset_string_storage ->
  match format with
  | 0 ->
      d_name_records ~count ~offset_string_storage >>= fun name_records ->
      return Value.Name.{
        name_records;
        lang_tags = None;
      }

  | 1 ->
      d_name_records ~count ~offset_string_storage >>= fun name_records ->
      d_lang_tag_records ~offset_string_storage >>= fun lang_tags ->
      return Value.Name.{
        name_records;
        lang_tags = Some(lang_tags);
      }

  | _ ->
      err @@ Error.UnknownTableVersion(!% format)


let get (src : source) =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_name >>= fun (offset_name, _length) ->
  d_name |> DecodeOperation.run common.core offset_name
