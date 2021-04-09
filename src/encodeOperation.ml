
include EncodeOperationCore


open Open


let e_16bits (bits : bool list) =
  let rec aux (p : int) (n : int) (bits : bool list) =
    match bits with
    | [] ->
        e_uint16 n

    | bit :: bits ->
        let n = if bit then n + p else n in
        aux (p * 2) n bits
  in
  aux 1 0 bits


let rec e_list (enc : 'a -> unit encoder) =
  function
  | [] ->
      return ()

  | x :: xs ->
      enc x >>= fun () ->
      e_list enc xs
