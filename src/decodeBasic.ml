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

type single_or_collection =
  | Single     of source
  | Collection of source list

module Decoder : sig
  type 'a t
  val return : 'a -> 'a t
  val err : Error.t -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val mapM : ('a -> 'b t) -> 'a list -> ('b list) t
  val run : common_source -> int -> 'a t -> 'a ok
  val current : offset t
  val seek : offset -> unit t
  val d_uint8 : int t
  val d_uint32 : wint t
end = struct
  type state = {
    source   : common_source;
    position : offset;
  }

  type 'a t = state -> (state * 'a) ok


  let return v =
    fun state -> Ok((state, v))


  let err e =
    fun _ -> Error(e)


  let ( >>= ) d df state =
    let open ResultMonad in
    d state >>= fun (state, v) ->
    df v state


  let mapM (df : 'a -> 'b t) (vs : 'a list) : ('b list) t =
    fun state ->
      let open ResultMonad in
      let res =
        vs |> List.fold_left (fun res v ->
          res >>= fun (state, acc) ->
          df v state >>= fun (state, y) ->
          return @@ (state, Alist.extend acc y)
        ) (Ok((state, Alist.empty)))
      in
      res >>= fun (state, acc) ->
      return @@ (state, Alist.to_list acc)


  let run common offset d =
    let open ResultMonad in
    let state = {source = common; position = offset} in
    d state >>= fun (_, v) ->
    return v


  let current : offset t =
    fun state -> Ok((state, state.position))


  let seek (offset : offset) : unit t =
    fun state ->
      if offset > state.source.max then
        Error(InvalidOffset(offset))
      else
        Ok(({ state with position = offset }, ()))


  let miss (state : state) (count : int) : bool =
    state.source.max < state.position + count


  let advance (state : state) (count : int) : state =
    { state with position = state.position + count }


  let raw_byte (s : string) (offset : offset) : char =
    String.get s offset


  let d_uint8 : int t =
    let open ResultMonad in
    fun state ->
      if miss state 1 then
        err UnexpectedEnd
      else
        let n = Char.code (raw_byte state.source.data state.position) in
        return (advance state 1, n)


  let d_uint32 : wint t =
    let open ResultMonad in
    fun state ->
      if miss state 4 then
        err UnexpectedEnd
      else
        let s = state.source.data in
        let pos = state.position in
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
        return (advance state 4, n)

end


type 'a decoder = 'a Decoder.t


let d_uint32_int : int decoder =
  let open Decoder in
  d_uint32 >>= fun n ->
  return @@ WideInt.to_int n


let d_repeat : 'a. int -> 'a decoder -> ('a list) decoder =
fun count d ->
  let open Decoder in
  let rec aux acc i =
    if i <= 0 then
      return @@ Alist.to_list acc
    else
      d >>= fun v ->
      aux (Alist.extend acc v) (i - 1)
  in
  aux Alist.empty count


let d_tag : Value.Tag.t decoder =
  let open Decoder in
  d_uint32 >>= fun n ->
  return @@ Value.Tag.of_wide_int n


type format_version_result =
  | InitTtf
  | InitCff
  | InitCollection


let d_format_version : format_version_result decoder =
  let open Decoder in
  d_tag >>= fun tag ->
  let open Value.Tag in
  if equal tag format_version_OTTO then
    return InitCff
  else if equal tag format_version_true || equal tag format_version_1_0 then
    return InitTtf
  else if equal tag format_version_ttcf then
    return InitCollection
  else
    err @@ UnknownFormatVersion(tag)


let d_long_offset_list : (offset list) decoder =
  let open Decoder in
  d_uint32_int >>= fun count ->
  d_repeat count d_uint32_int


let d_ttc_header_offset_list : (offset list) decoder =
  let open Decoder in
  d_uint32 >>= fun ttc_version ->
  if WideInt.equal ttc_version (!%% 0x00010000L) || WideInt.equal ttc_version (!%% 0x00020000L) then
    d_long_offset_list
  else
    err @@ UnknownTtcVersion(ttc_version)


let source_of_string (s : string) : single_or_collection ok =
  let open Decoder in
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
