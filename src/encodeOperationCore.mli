
open Basic
open EncodeBasic

module Open : sig
  (** The type for encoders by running which one can gain a string. *)
  type encoder
end

open Open

(** [run enc] runs the given encoder [enc] and returns the resulting byte sequence as a string value.
    The option [?initial_capacity] is the size of the initial allocation for the byte sequence,
    which will be internally used for creating a buffer (i.e. a value of type [Buffer.t]). *)
val run : ?initial_capacity:int -> encoder -> (string, Error.t) result

(** The sequential composition of encoders. *)
val ( >> ) : encoder -> encoder -> encoder

(** [e_byte ch] adds the character [ch]. *)
val e_byte : char -> encoder

(** [e_bytes s] adds the string [s]. *)
val e_bytes : string -> encoder

(** [e_pads n] adds the [n]-byte sequence of [0x00]. *)
val e_pad : int -> encoder

val e_uint8 : int -> encoder

val e_int8 : int -> encoder

val e_uint16 : int -> encoder

val e_int16 : int -> encoder

val e_uint24 : int -> encoder

val e_uint32 : wint -> encoder

val e_int32 : wint -> encoder
