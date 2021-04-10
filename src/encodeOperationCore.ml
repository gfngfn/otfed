
open Basic
open EncodeBasic


module Open = struct
  type 'a encoder = Buffer.t -> ('a, Error.t) result
end


open Open


let run : 'a. ?initial_capacity:int -> 'a encoder -> (string * 'a, Error.t) result =
fun ?initial_capacity:(initial_capacity = 65535) enc ->
  let open ResultMonad in
  let buf = Buffer.create initial_capacity in
  let res = enc buf in
  match res with
  | Ok(v)    -> return @@ (Buffer.contents buf, v)
  | Error(e) -> err @@ e


let ( >>= ) (enc1 : 'a encoder) (encf2 : 'a -> 'b encoder) : 'b encoder =
fun buf ->
  let open ResultMonad in
  enc1 buf >>= fun v ->
  encf2 v buf


let return (v : 'a) : 'a encoder =
fun _buf ->
  let open ResultMonad in
  return v


let err (e : Error.t) : 'a encoder =
fun _buf ->
  let open ResultMonad in
  err @@ e


let current : int encoder =
fun buf ->
  let open ResultMonad in
  return @@ Buffer.length buf


let e_byte (ch : char) : unit encoder =
fun buf ->
  let open ResultMonad in
  Buffer.add_char buf ch;
  return ()


let e_bytes (s : string) : unit encoder =
fun buf ->
  let open ResultMonad in
  Buffer.add_string buf s;
  return ()


let e_pad (count : int) : unit encoder =
fun buf ->
  let open ResultMonad in
  Buffer.add_string buf (String.make count (Char.chr 0));
  return ()


let encode_uint8_unsafe buf u =
  let open ResultMonad in
  Buffer.add_char buf (Char.chr u);
  return ()


let encode_uint16_unsafe buf u =
  let open ResultMonad in
  let b0 = u lsr 8 in
  let b1 = u - (b0 lsl 8) in
  Buffer.add_char buf (Char.chr b0);
  Buffer.add_char buf (Char.chr b1);
  return ()


let encode_uint24_unsafe buf u =
  let open ResultMonad in
  let b0 = u lsr 16 in
  let r0 = u - (b0 lsl 16) in
  let b1 = r0 lsr 8 in
  let b2 = r0 - (b1 lsl 8) in
  Buffer.add_char buf (Char.chr b0);
  Buffer.add_char buf (Char.chr b1);
  Buffer.add_char buf (Char.chr b2);
  return ()


let encode_uint32_unsafe buf u =
  let open ResultMonad in
  let open WideInt in
  let b0 = u lsr 24 in
  let r0 = u -% (b0 lsl 24) in
  let b1 = r0 lsr 16 in
  let r1 = r0 -% (b1 lsl 16) in
  let b2 = r1 lsr 8 in
  let b3 = r1 -% (b2 lsl 8) in
  Buffer.add_char buf (to_byte b0);
  Buffer.add_char buf (to_byte b1);
  Buffer.add_char buf (to_byte b2);
  Buffer.add_char buf (to_byte b3);
  return ()


let e_uint8 (n : int) : unit encoder =
fun buf ->
  let open ResultMonad in
  if 0 <= n && n < 256 then
    encode_uint8_unsafe buf n
  else
    err @@ Error.NotEncodableAsUint8(n)


let e_int8 (n : int) : unit encoder =
fun buf ->
  let open ResultMonad in
  if -128 <= n && n < 128 then
    let u = if n < 0 then n + 256 else n in
    encode_uint8_unsafe buf u
  else
    err @@ Error.NotEncodableAsInt8(n)


let e_uint16 (n : int) : unit encoder =
fun buf ->
  let open ResultMonad in
  if 0 <= n && n < 65536 then
    encode_uint16_unsafe buf n
  else
    err @@ Error.NotEncodableAsUint16(n)


let e_int16 (n : int) : unit encoder =
fun buf ->
  let open ResultMonad in
  if -32768 <= n && n < 32768 then
    let u = if n < 0 then n + 65536 else n in
    encode_uint16_unsafe buf u
  else
    err @@ Error.NotEncodableAsInt16(n)


let e_uint24 (n : int) : unit encoder =
fun buf ->
  let open ResultMonad in
  if 0 <= n && n < 0x1000000 then
    encode_uint24_unsafe buf n
  else
    err @@ Error.NotEncodableAsUint24(n)


let e_uint32 (n : wint) : unit encoder =
fun buf ->
  let open ResultMonad in
  if WideInt.is_in_uint32 n then
    encode_uint32_unsafe buf n
  else
    err @@ Error.NotEncodableAsUint32(n)


let e_int32 (n : wint) : unit encoder =
fun buf ->
  let open ResultMonad in
  if WideInt.is_in_int32 n then
    let u =
      let open WideInt in
      if is_neg n then n +% (!%% 0x10000000L) else n
    in
    encode_uint32_unsafe buf u
  else
    err @@ Error.NotEncodableAsInt32(n)


let e_timestamp (n : wint) : unit encoder =
fun buf ->
  let open ResultMonad in
  let open WideInt in
  if is_in_int64 n then
    let u = if is_neg n then !% 0 else n in  (* temporary *)
    let q0 = u lsr 32 in
    let q1 = u -% (q0 lsl 32) in
    encode_uint32_unsafe buf q0 >>= fun () ->
    encode_uint32_unsafe buf q1
  else
    err @@ Error.NotEncodableAsTimestamp(n)
