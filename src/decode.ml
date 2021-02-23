
open Basic
open DecodeOperation


include DecodeBasic


let d_init_ttf core =
  d_structure >>= fun table_directory ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
    }
  in
  let ttf = {ttf_common = common} in
  return @@ (common, Ttf(ttf))


let d_init_cff (core : common_source_core) : source decoder =
  d_structure >>= fun table_directory ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
    }
  in
  let cff = {cff_common = common} in
  return @@ (common, Cff(cff))


let source_of_string (s : string) : single_or_collection ok =
  let core =
    {
      data = s;
      max  = String.length s - 1;
    }
  in
  let dec =
    d_format_version >>= fun format ->
    match format with
    | InitTtf ->
        d_init_ttf core >>= fun src ->
        return @@ Single(src)

    | InitCff ->
        d_init_cff core >>= fun src ->
        return @@ Single(src)

    | InitCollection ->
        d_ttc_header_offset_list >>= fun offsets ->
        offsets |> mapM (fun offset ->
          seek offset >>= fun () ->
          d_format_version >>= fun format ->
          match format with
          | InitTtf ->
              d_init_ttf core

          | InitCff ->
              d_init_cff core

          | InitCollection ->
              err LayeredTtc
        ) >>= fun srcs ->
        return @@ Collection(srcs)
  in
  run core 0 dec


module Intermediate = DecodeIntermediate


let seek_required_table common tag =
  let open ResultMonad in
  match common.table_directory |> TableDirectory.find_opt tag with
  | None    -> err @@ Error.MissingRequiredTable(tag)
  | Some(v) -> return @@ v


let cmap (common : common_source) : Intermediate.Cmap.t ok =
  let open ResultMonad in
  seek_required_table common Value.Tag.table_cmap >>= fun (offset, length) ->
  return @@ Intermediate.Cmap.make common.core offset length
