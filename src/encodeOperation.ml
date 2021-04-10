
open Basic


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


let mapM (enc : 'a -> 'b encoder) =
  let rec aux acc = function
    | [] ->
        return @@ Alist.to_list acc

    | x :: xs ->
        enc x >>= fun v ->
        aux (Alist.extend acc v) xs
  in
  aux Alist.empty


let e_list (enc : 'a -> 'b encoder) (xs : 'a list) =
  mapM enc xs >>= fun _ ->
  return ()


let pad_to_long_aligned : unit encoder =
  current >>= fun length ->
  match length mod 4 with
  | 0 -> return ()
  | n -> e_bytes (String.make n (Char.chr 0))
