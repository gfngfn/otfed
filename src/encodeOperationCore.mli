
open Basic
open EncodeBasic

module Open : sig
  (** The type for encoders by running which one can gain a string and a value of type ['a]. *)
  type 'a encoder
end

open Open

(** [run enc] runs the given encoder [enc] and returns the resulting byte sequence as a string value.
    The option [?initial_capacity] is the size of the initial allocation for the byte sequence,
    which will be internally used for creating a buffer (i.e. a value of type [Buffer.t]). *)
val run : ?initial_capacity:int -> 'a encoder -> (string * 'a, Error.t) result

(** The sequential composition of encoders. *)
val ( >>= ) : 'a encoder -> ('a -> 'b encoder) -> 'b encoder

(** [return v] makes an encoder that writes nothing and simply returns [v]. *)
val return : 'a -> 'a encoder

(** [err e] makes an encoder that immediately halts by returning the error [e]. *)
val err : Error.t -> 'a encoder

(** Returns how many bytes have been written so far by the current run. *)
val current : int encoder

(** [e_byte ch] adds the character [ch]. *)
val e_byte : char -> unit encoder

(** [e_bytes s] adds the string [s]. *)
val e_bytes : string -> unit encoder

(** [e_pads n] adds the [n]-byte sequence of [0x00]. *)
val e_pad : int -> unit encoder

val e_uint8 : int -> unit encoder

val e_int8 : int -> unit encoder

val e_uint16 : int -> unit encoder

val e_int16 : int -> unit encoder

val e_uint24 : int -> unit encoder

val e_uint32 : wint -> unit encoder

val e_int32 : wint -> unit encoder
