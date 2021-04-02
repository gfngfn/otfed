
open Basic
open Value
open DecodeOperation.Open


include DecodeBasic


let fetch_loc_format core table_directory =
  let open ResultMonad in
  DecodeOperation.seek_required_table table_directory Tag.table_head >>= fun (offset, _length) ->
  let open DecodeOperation in
  d_loc_format |> run core (offset + 50)


let fetch_num_glyphs core table_directory =
  let open ResultMonad in
  DecodeOperation.seek_required_table table_directory Tag.table_maxp >>= fun (offset, _length) ->
  let open DecodeOperation in
  d_uint16 |> run core (offset + 4)


let fetch_num_h_metrics core table_directory =
  let open ResultMonad in
  DecodeOperation.seek_required_table table_directory Tag.table_hhea >>= fun (offset, _length) ->
  let open DecodeOperation in
  d_uint16 |> run core (offset + 34)


let d_init_ttf core =
  let open DecodeOperation in
  d_structure >>= fun table_directory ->
  transform_result (fetch_loc_format core table_directory) >>= fun loc_format ->
  transform_result (fetch_num_glyphs core table_directory) >>= fun num_glyphs ->
  transform_result (fetch_num_h_metrics core table_directory) >>= fun num_h_metrics ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
      loc_format      = loc_format;
      num_glyphs      = num_glyphs;
      num_h_metrics   = num_h_metrics;
    }
  in
  let ttf = {ttf_common = common} in
  return @@ (common, Ttf(ttf))


let d_init_cff (core : common_source_core) : source decoder =
  let open DecodeOperation in
  d_structure >>= fun table_directory ->
  transform_result (fetch_loc_format core table_directory) >>= fun loc_format ->
  transform_result (fetch_num_glyphs core table_directory) >>= fun num_glyphs ->
  transform_result (fetch_num_h_metrics core table_directory) >>= fun num_h_metrics ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
      loc_format      = loc_format;
      num_glyphs      = num_glyphs;
      num_h_metrics   = num_h_metrics;
    }
  in
  let cff = {cff_common = common} in
  return @@ (common, Cff(cff))


let source_of_string (s : string) : single_or_collection ok =
  let core =
    {
      data = s;
      max  = String.length s;
    }
  in
  let dec =
    let open DecodeOperation in
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
  DecodeOperation.run core 0 dec


let tables (common : common_source) : Value.Tag.t set =
  let acc =
    TableDirectory.fold (fun tag _ acc ->
      Alist.extend acc tag
    ) common.table_directory Alist.empty
  in
  acc |> Alist.to_list


module Intermediate = DecodeIntermediate


include DecodeTtf

include DecodeCff

module ForTest = struct
  type charstring_data = DecodeBasic.charstring_data
  type subroutine_index = DecodeBasic.subroutine_index
  type 'a decoder = 'a DecodeOperation.Open.decoder

  let run s d =
    DecodeOperation.run { data = s; max = String.length s } 0 d

  let d_glyf =
    d_glyf

  let run_d_charstring ~gsubr_index ~lsubr_index data ~start ~charstring_length =
    let cstate = initial_charstring_state charstring_length in
    let dec =
      let open DecodeOperation in
      d_charstring { gsubr_index; lsubr_index } cstate >>= fun (_, opacc) ->
      return @@ Alist.to_list opacc
    in
    dec |> DecodeOperation.run { data = data; max = String.length data } start
end
