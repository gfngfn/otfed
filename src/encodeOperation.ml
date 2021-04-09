
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


let rec e_list (enc : 'a -> unit encoder) =
  function
  | [] ->
      return ()

  | x :: xs ->
      enc x >>= fun () ->
      e_list enc xs
