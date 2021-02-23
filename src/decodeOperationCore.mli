
open Basic
open DecodeBasic

type 'a decoder
val return : 'a -> 'a decoder
val err : Error.t -> 'a decoder
val ( >>= ) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
val mapM : ('a -> 'b decoder) -> 'a list -> ('b list) decoder
val run : common_source_core -> int -> 'a decoder -> 'a ok
val current : offset decoder
val seek : offset -> unit decoder
val d_skip : int -> unit decoder
val d_uint8 : int decoder
val d_uint16 : int decoder
val d_uint32 : wint decoder
