
open Basic
open Value
open EncodeBasic


include EncodeOperationCore


open Open


let rec encode_bits (enc : int -> unit encoder) (p : int) (n : int) (bits : bool list) =
  match bits with
  | [] ->
      enc n

  | bit :: bits ->
      let n = if bit then n + p else n in
      encode_bits enc (p * 2) n bits


let e_8bits (bits : bool list) =
  encode_bits e_uint8 1 0 bits


let e_16bits (bits : bool list) =
  encode_bits e_uint16 1 0 bits


let e_f2dot14 (x : float) =
  e_int16 (int_of_float (x *. 16384.))


let e_bmp_code_point (uch : Uchar.t) =
  let n = Stdlib.min (Uchar.to_int uch) 0xFFFF in
  e_uint16 n


let e_tag (tag : Tag.t) =
  e_bytes (Tag.to_string tag)


let mapM (enc : 'a -> 'b encoder) =
  let rec aux acc = function
    | [] ->
        return @@ Alist.to_list acc

    | x :: xs ->
        enc x >>= fun v ->
        aux (Alist.extend acc v) xs
  in
  aux Alist.empty


let foldM (enc : 'b -> 'a -> 'b encoder) (xs : 'a list) (acc : 'b) =
  let rec aux acc = function
    | [] ->
        return acc

    | x :: xs ->
        enc acc x >>= fun acc ->
        aux acc xs
  in
  aux acc xs


let transform_result (res : 'a ok) : 'a encoder =
  match res with
  | Ok(v)    -> return v
  | Error(e) -> err e


let e_list (enc : 'a -> 'b encoder) (xs : 'a list) =
  mapM enc xs >>= fun _ ->
  return ()


let e_paddings (len : int) : unit encoder =
  e_bytes (String.make len (Char.chr 0))


let pad_to_long_aligned : unit encoder =
  current >>= fun length ->
  match length mod 4 with
  | 0 -> return ()
  | n -> e_bytes (String.make n (Char.chr 0))
