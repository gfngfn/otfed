
include DecodeBasic


let source_of_string (s : string) : single_or_collection ok =
  let open DecodeOperation in
  let common =
    {
      data = s;
      max  = String.length s - 1;
    }
  in
  let dec =
    d_format_version >>= fun format ->
    match format with
    | InitTtf ->
        let ttf = {ttf_common = common} in
        return @@ Single(common, Ttf(ttf))

    | InitCff ->
        let cff = {cff_common = common} in
        return @@ Single(common, Cff(cff))

    | InitCollection ->
        d_ttc_header_offset_list >>= fun offsets ->
        offsets |> mapM (fun offset ->
          seek offset >>= fun () ->
          d_format_version >>= fun format ->
          match format with
          | InitTtf ->
              let ttf = {ttf_common = common} in
              return @@ (common, Ttf(ttf))

          | InitCff ->
              let cff = {cff_common = common} in
              return @@ (common, Cff(cff))

          | InitCollection ->
              err LayeredTtc
        ) >>= fun srcs ->
        return @@ Collection(srcs)
  in
  run common 0 dec


module Intermediate = DecodeIntermediate


let cmap (_ : common_source) : Intermediate.Cmap.t ok =
  failwith "TODO"
