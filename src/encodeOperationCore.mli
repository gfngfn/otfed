
open EncodeBasic

module Open : sig
  type encoder
end

open Open

val run : ?initial_capacity:int -> encoder -> (string, Error.t) result

val ( >> ) : encoder -> encoder -> encoder

val e_byte : char -> encoder

val e_uint8 : int -> encoder

val e_uint16 : int -> encoder
