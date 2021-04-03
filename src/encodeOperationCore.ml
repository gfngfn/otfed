
open Basic
open EncodeBasic


module Open = struct
  type encoder = Buffer.t -> (unit, Error.t) result
end


open Open


let run : 'a. ?initial_capacity:int -> encoder -> (string, Error.t) result =
fun ?initial_capacity:(initial_capacity = 65535) enc ->
  let open ResultMonad in
  let buf = Buffer.create initial_capacity in
  let res = enc buf in
  match res with
  | Ok(())   -> return @@ Buffer.contents buf
  | Error(e) -> err @@ e


let ( >> ) (enc1 : encoder) (enc2 : encoder) =
fun buf ->
  let open ResultMonad in
  let res1 = enc1 buf in
  match res1 with
  | Ok(())   -> enc2 buf
  | Error(e) -> err @@ e


let e_byte (ch : char) : encoder =
fun buf ->
  let open ResultMonad in
  Buffer.add_char buf ch;
  return ()


let e_bytes (s : string) : encoder =
fun buf ->
  let open ResultMonad in
  Buffer.add_string buf s;
  return ()


let e_pad (count : int) : encoder =
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


let e_uint8 (n : int) : encoder =
fun buf ->
  let open ResultMonad in
  if 0 <= n && n < 256 then
    encode_uint8_unsafe buf n
  else
    err @@ Error.NotEncodableAsUint8(n)


let e_int8 (n : int) : encoder =
fun buf ->
  let open ResultMonad in
  if -128 <= n && n < 128 then
    let u = if n < 0 then n + 128 else n in
    encode_uint8_unsafe buf u
  else
    err @@ Error.NotEncodableAsInt8(n)


let e_uint16 (n : int) : encoder =
fun buf ->
  let open ResultMonad in
  if 0 <= n && n < 65536 then
    encode_uint16_unsafe buf n
  else
    err @@ Error.NotEncodableAsUint8(n)


let e_int16 (n : int) : encoder =
fun buf ->
  let open ResultMonad in
  if -32768 <= n && n <= 32768 then
    let u = if n < 0 then n + 32768 else n in
    encode_uint16_unsafe buf u
  else
    err @@ Error.NotEncodableAsInt16(n)
