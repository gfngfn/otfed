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


let d_tag (dec : decoder) : (decoder * Value.Tag.t) ok =
  let open ResultMonad in
  d_uint32 dec >>= fun (dec, n) ->
  return (dec, Value.Tag.of_wide_int n)


let d_format_version (dec : decoder) =
  let open ResultMonad in
  d_tag dec >>= fun (_dec, tag) ->
  let open Value.Tag in
  if equal tag format_version_OTTO then
    failwith "TODO: initialize CFF"
  else if equal tag format_version_true || equal tag format_version_1_0 then
    failwith "TODO: initialize TTF"
  else if equal tag format_version_ttcf then
    failwith "TODO: initialize TTC"
  else
    err @@ UnknownFormatVersion(tag)



let source_of_string (s : string) : source ok =
  let open ResultMonad in
  let cmsrc =
    {
      data = s;
      max  = String.length s - 1;
    }
  in
  let spsrc = failwith "TODO" in
  return (cmsrc, spsrc)
