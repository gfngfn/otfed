
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
fun (buf : Buffer.t) ->
  let open ResultMonad in
  let res1 = enc1 buf in
  match res1 with
  | Ok(())   -> enc2 buf
  | Error(e) -> err @@ e


let e_byte (ch : char) : encoder =
fun (buf : Buffer.t) ->
  let open ResultMonad in
  Buffer.add_char buf ch;
  return ()


let e_uint8 (n : int) : encoder =
fun (buf : Buffer.t) ->
  let open ResultMonad in
  if 0 <= n && n < 256 then begin
    Buffer.add_char buf (Char.chr n);
    return ()
  end else
    err @@ Error.NotEncodableAsUint8(n)


let e_uint16 (n : int) : encoder =
fun (buf : Buffer.t) ->
  let open ResultMonad in
  if 0 <= n && n < 65536 then begin
    let b0 = n lsr 8 in
    let b1 = n - (b0 lsl 8) in
    Buffer.add_char buf (Char.chr b0);
    Buffer.add_char buf (Char.chr b1);
    return ()
  end else
    err @@ Error.NotEncodableAsUint8(n)
