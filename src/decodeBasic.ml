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


let raw_byte (dec : decoder) : char =
  String.get dec.source.data dec.position


let d_uint8 (dec : decoder) : (decoder * int) ok =
  let open ResultMonad in
  if miss dec 1 then
    err UnexpectedEnd
  else
    let byte = raw_byte dec in
    let dec = advance dec 1 in
    return (dec, Char.code byte)


let d_uint32 (dec : decoder) : (decoder * int) ok =
  let open ResultMonad in
  if miss dec 4 then
    err UnexpectedEnd
  else
    failwith "TODO"


let d_format_version (d : decoder) =
  let open ResultMonad in
  d_uint32 d >>= fun (_d, _tag_int) ->
  failwith "TODO"


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
