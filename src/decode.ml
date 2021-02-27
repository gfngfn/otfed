
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


let tables (common : common_source) : Value.Tag.t set =
  let acc =
    TableDirectory.fold (fun tag _ acc ->
      Alist.extend acc tag
    ) common.table_directory Alist.empty
  in
  acc |> Alist.to_list


module Intermediate = DecodeIntermediate


let cmap (common : common_source) : Intermediate.Cmap.t ok =
  let open ResultMonad in
  seek_required_table common Value.Tag.table_cmap >>= fun (offset, length) ->
  return @@ Intermediate.Cmap.make common.core offset length


let head (common : common_source) : Value.Head.t ok =
  let open ResultMonad in
  seek_required_table common Value.Tag.table_head >>= fun (offset, _length) ->
  let dec =
    let open DecodeOperation in
    d_uint32 >>= fun version ->
    if version <> !%% 0x00010000L then
      err @@ UnknownTableVersion(version)
    else
      d_uint32     >>= fun font_revision ->
      d_skip 8     >>= fun () -> (* Skips `checkSumAdjustement` and `magicNumber`. *)
      d_uint16     >>= fun flags ->
      d_uint16     >>= fun units_per_em ->
      d_timestamp  >>= fun created ->
      d_timestamp  >>= fun modified ->
      d_int16      >>= fun xmin ->
      d_int16      >>= fun ymin ->
      d_int16      >>= fun xmax ->
      d_int16      >>= fun ymax ->
      d_uint16     >>= fun mac_style ->
      d_uint16     >>= fun lowest_rec_ppem ->
      d_skip 2     >>= fun () -> (* Skips `fontDirectionHint`. *)
      d_loc_format >>= fun index_to_loc_format ->
      return Value.Head.{
        font_revision;
        flags;
        units_per_em;
        created;
        modified;
        xmin;
        ymin;
        xmax;
        ymax;
        mac_style;
        lowest_rec_ppem;
        index_to_loc_format;
      }
  in
  DecodeOperation.run common.core offset dec
