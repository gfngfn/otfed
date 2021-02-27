(** The core operations for byte sequence decoders.
    This module is intended to be used only by [DecodeOperation];
    [DecodeOperation] extends this module with higher-level decoding operations. *)

open Basic
open DecodeBasic

type 'a decoder
val return : 'a -> 'a decoder
val err : Error.t -> 'a decoder
val ( >>= ) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
val transform_result : 'a ok -> 'a decoder
val mapM : ('a -> 'b decoder) -> 'a list -> ('b list) decoder
val run : common_source_core -> int -> 'a decoder -> 'a ok
val current : offset decoder
val seek : offset -> unit decoder
val d_skip : int -> unit decoder
val d_uint8 : int decoder
val d_int8 : int decoder
val d_uint16 : int decoder
val d_int16 : int decoder
val d_uint32 : wint decoder
