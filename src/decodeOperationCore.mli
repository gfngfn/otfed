(** The core operations for byte sequence decoders.
    This module is intended to be used only by [DecodeOperation];
    [DecodeOperation] extends this module with higher-level decoding operations. *)

open Basic
open Value
open DecodeBasic

(** The type for decoders by running which one can gain a value of type ['a]. *)
type 'a decoder

(** The monadic [pure] function of decoders.
    [return v] makes a decoder that always produces [v] without moving its position. *)
val return : 'a -> 'a decoder

(** [err e] makes a decoder that always fails with reason [e]. *)
val err : Error.t -> 'a decoder

(** The monadic binding operator for decoders. *)
val ( >>= ) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder

(** [transform_result (Ok(v))] is equivalent to [return v], while [transform_result (Error(e))] is to [err e]. *)
val transform_result : 'a ok -> 'a decoder

(** The monadic list-mapping function for decoders. *)
val mapM : ('a -> 'b decoder) -> 'a list -> ('b list) decoder

(** [run core offset dec] starts to decode the source [core] at the position [offset]
    by using [dec] and returns the resulting value. *)
val run : common_source_core -> offset -> 'a decoder -> 'a ok

(** Returns the current position. *)
val current : offset decoder

(** [seek offset] jumps to the position [offset]. *)
val seek : offset -> unit decoder

(** [d_bytes n] reads [n] bytes as a string. *)
val d_bytes : int -> string decoder

(** Advances the position by the given number of bytes. *)
val d_skip : int -> unit decoder

(** Reads a byte as an unsigned integer. *)
val d_uint8 : int decoder

(** Reads a byte as a signed integer. *)
val d_int8 : int decoder

(** Reads 2 bytes as an unsigned integer. *)
val d_uint16 : int decoder

(** Reads 2 bytes as a signed integer. *)
val d_int16 : int decoder

(** Reads 3 bytes as an unsigned integer. *)
val d_uint24 : int decoder

(** Reads 4 bytes as an unsigned integer.
    Due to the limitation of the range representable by the type [int] under 32-bit environments,
    this function returns [wint]-typed values. *)
val d_uint32 : wint decoder

(** Reads 8 bytes as a Unix timestamp. *)
val d_timestamp : timestamp decoder
