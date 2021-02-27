(* `DecodeOperationCore` encapsulates the minimal set of decoding operations. *)

open Basic
open DecodeBasic


type state = {
  source   : common_source_core;
  position : offset;
}

type 'a decoder = state -> (state * 'a) ok


let return v =
  fun state -> Ok((state, v))


let err e =
  fun _ -> Error(e)


let ( >>= ) d df state =
  let open ResultMonad in
  d state >>= fun (state, v) ->
  df v state


let mapM df vs =
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


let transform_result (res : 'a ok) : 'a decoder =
  match res with
  | Ok(v)    -> return v
  | Error(e) -> err e


let run common offset d =
  let open ResultMonad in
  let state = {source = common; position = offset} in
  d state >>= fun (_, v) ->
  return v


let current : offset decoder =
  fun state -> Ok((state, state.position))


let seek (offset : offset) : unit decoder =
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


let d_skip (n : int) : unit decoder =
  let open ResultMonad in
  fun state ->
    if miss state n then
      err Error.UnexpectedEnd
    else
      return (advance state n, ())


let d_uint8 : int decoder =
  let open ResultMonad in
  fun state ->
    if miss state 1 then
      err Error.UnexpectedEnd
    else
      let n = Char.code (raw_byte state.source.data state.position) in
      return (advance state 1, n)


let d_int8 : int decoder =
  d_uint8 >>= fun n ->
  return (if n > 0x7F then n - 0x100 else n)


let d_uint16 : int decoder =
  let open ResultMonad in
  fun state ->
    if miss state 2 then
      err Error.UnexpectedEnd
    else
      let s = state.source.data in
      let pos = state.position in
      let by0 = raw_byte s pos in
      let by1 = raw_byte s (pos + 1) in
      let n =
        let w0 = Char.code by0 lsl 8 in
        let w1 = Char.code by1 in
        w0 lor w1
      in
      return (advance state 2, n)


let d_int16 : int decoder =
  d_uint16 >>= fun n ->
  return (if n > 0x7FFF then n - 0x10000 else n)


let d_uint32 : wint decoder =
  let open ResultMonad in
  fun state ->
    if miss state 4 then
      err Error.UnexpectedEnd
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
