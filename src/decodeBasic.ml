(*  `DecodeBasic` handles entries that can be seen from
    any place within `Otfed.Decode`,
    and is included by `Otfed.Decode`. *)

open Basic

module Error = DecodeError

open Error

type 'a ok = ('a, Error.t) result

type common_source = {
  data : string;
  max  : offset;
}

type ttf_source = {
  ttf_common : common_source;
}

type cff_source = {
  cff_common : common_source;
}

type specific_source =
  | Ttf of ttf_source
  | Cff of cff_source

type source = common_source * specific_source

type decoder = {
  source   : common_source;
  position : offset;
}


let make_decoder (common : common_source) (offset : offset) : decoder ok =
  let open ResultMonad in
  return @@ {
    source   = common;
    position = offset;
  }


let current (d : decoder) : offset =
  d.position


let seek (ofs : offset) (d : decoder) : decoder ok =
  let open ResultMonad in
  if ofs > d.source.max then
    err @@ InvalidOffset(ofs)
  else
    return { d with position = ofs }


let miss (dec : decoder) (count : int) : bool =
  dec.source.max < dec.position + count


let advance (dec : decoder) (count : int) : decoder =
  { dec with position = dec.position + count }


let raw_byte (s : string) (offset : offset) : char =
  String.get s offset


let d_uint8 (dec : decoder) : (decoder * int) ok =
  let open ResultMonad in
  if miss dec 1 then
    err UnexpectedEnd
  else
    let n = Char.code (raw_byte dec.source.data dec.position) in
    let dec = advance dec 1 in
    return (dec, n)


let d_uint32 (dec : decoder) : (decoder * wint) ok =
  let open ResultMonad in
  if miss dec 4 then
    err UnexpectedEnd
  else
    let s = dec.source.data in
    let pos = dec.position in
    let by0 = raw_byte s pos in
    let by1 = raw_byte s (pos + 1) in
    let by2 = raw_byte s (pos + 2) in
    let by3 = raw_byte s (pos + 3) in
    let n =
      let open WideInt in
      let w0 = of_byte by0 lsl 24 in
      let w1 = of_byte by1 lsl 16 in
      let w2 = of_byte by2 lsl 8 in
      let w3 = of_byte by3 in
      w0 lor w1 lor w2 lor w3
    in
    let dec = advance dec 4 in
    return (dec, n)


let d_uint32_int (dec : decoder) : (decoder * int) ok =
  let open ResultMonad in
  d_uint32 dec >>= fun (dec, n) ->
  return (dec, WideInt.to_int n)


let d_repeat : 'a. int -> (decoder -> (decoder * 'a) ok) -> decoder -> (decoder * 'a list) ok =
fun count df dec ->
  let open ResultMonad in
  let rec aux dec acc i =
    if i <= 0 then
      return (dec, List.rev acc)
    else
      df dec >>= fun (dec, v) ->
      aux dec (v :: acc) (i - 1)
  in
  aux dec [] count


let d_tag (dec : decoder) : (decoder * Value.Tag.t) ok =
  let open ResultMonad in
  d_uint32 dec >>= fun (dec, n) ->
  return (dec, Value.Tag.of_wide_int n)


type format_version_result =
  | InitTtf
  | InitCff
  | InitCollection


let d_format_version (dec : decoder) : (decoder * format_version_result) ok =
  let open ResultMonad in
  d_tag dec >>= fun (dec, tag) ->
  let open Value.Tag in
  if equal tag format_version_OTTO then
    return (dec, InitCff)
  else if equal tag format_version_true || equal tag format_version_1_0 then
    return (dec, InitTtf)
  else if equal tag format_version_ttcf then
    return (dec, InitCollection)
  else
    err @@ UnknownFormatVersion(tag)


let d_long_offset_list (dec : decoder) : (decoder * offset list) ok =
  let open ResultMonad in
  d_uint32_int dec >>= fun (dec, count) ->
  d_repeat count d_uint32_int dec


let d_ttc_header_offset_list (dec : decoder) : (decoder * offset list) ok =
  let open ResultMonad in
  d_uint32 dec >>= fun (dec, ttc_version) ->
  if WideInt.equal ttc_version (!%% 0x00010000L) || WideInt.equal ttc_version (!%% 0x00020000L) then
    d_long_offset_list dec
  else
    err @@ UnknownTtcVersion(ttc_version)


type single_or_collection =
  | Single     of source
  | Collection of source list


let source_of_string (s : string) : single_or_collection ok =
  let open ResultMonad in
  let common =
    {
      data = s;
      max  = String.length s - 1;
    }
  in
  make_decoder common 0 >>= fun dec ->
  d_format_version dec >>= fun (dec, format) ->
  match format with
  | InitTtf ->
      let ttf = {ttf_common = common} in
      return @@ Single(common, Ttf(ttf))

  | InitCff ->
      let cff = {cff_common = common} in
      return @@ Single(common, Cff(cff))

  | InitCollection ->
      d_ttc_header_offset_list dec >>= fun (_dec, offsets) ->
      offsets |> mapM (fun offset ->
        make_decoder common offset >>= fun dec ->
        d_format_version dec >>= fun (_dec, format) ->
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
