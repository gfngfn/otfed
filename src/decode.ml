
open Basic
open DecodeOperation.Open


include DecodeTable

include DecodeBasic


let fetch_num_glyphs core table_directory =
  let open ResultMonad in
  DecodeOperation.seek_required_table table_directory Value.Tag.table_maxp >>= fun (offset, _length) ->
  let open DecodeOperation in
  d_uint16 |> run core (offset + 4)


let d_init_ttf (core : common_source_core) : source decoder =
  let open DecodeOperation in
  d_structure >>= fun table_directory ->
  transform_result (Head.fetch_loc_format core table_directory)    >>= fun loc_format ->
  transform_result (fetch_num_glyphs core table_directory)         >>= fun num_glyphs ->
  transform_result (Hhea.fetch_num_h_metrics core table_directory) >>= fun num_h_metrics ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
      loc_format      = loc_format;
      num_glyphs      = num_glyphs;
      num_h_metrics   = num_h_metrics;
    }
  in
  let ttf = { ttf_common = common } in
  return @@ Ttf(ttf)


let d_init_cff (core : common_source_core) : source decoder =
  let open DecodeOperation in
  d_structure >>= fun table_directory ->
  transform_result (Head.fetch_loc_format core table_directory)    >>= fun loc_format ->
  transform_result (fetch_num_glyphs core table_directory)         >>= fun num_glyphs ->
  transform_result (Hhea.fetch_num_h_metrics core table_directory) >>= fun num_h_metrics ->
  transform_result (Cff.fetch_cff_specific core table_directory)   >>= fun cff_specific ->
  let common =
    {
      core            = core;
      table_directory = table_directory;
      loc_format      = loc_format;
      num_glyphs      = num_glyphs;
      num_h_metrics   = num_h_metrics;
    }
  in
  let cff =
    {
      cff_common   = common;
      cff_specific = cff_specific;
    }
  in
  return @@ Cff(cff)


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


let tables (src : source) : Value.Tag.t set =
  let common = get_common_source src in
  let acc =
    TableDirectory.fold (fun tag _ acc ->
      Alist.extend acc tag
    ) common.table_directory Alist.empty
  in
  acc |> Alist.to_list


module ForTest = struct
  type charstring_data = DecodeBasic.charstring_data
  type subroutine_index = DecodeBasic.subroutine_index

  module DecodeOperation = DecodeOperation

  type 'a decoder = 'a DecodeOperation.Open.decoder

  let run s d =
    DecodeOperation.run { data = s; max = String.length s } 0 d

  let d_glyf =
    Ttf.d_glyf

  let chop_two_bytes =
    DecodeOperation.ForTest.chop_two_bytes

  let run_d_charstring ~gsubr_index ~lsubr_index data ~start ~charstring_length =
    let cstate = Cff.initial_charstring_state charstring_length in
    let dec =
      let open DecodeOperation in
      Cff.d_charstring { gsubr_index; lsubr_index } cstate >>= fun (_, opacc) ->
      return @@ Alist.to_list opacc
    in
    dec |> DecodeOperation.run { data = data; max = String.length data } start
end
