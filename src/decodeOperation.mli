
open Basic
open DecodeBasic

type 'a decoder
val return : 'a -> 'a decoder
val err : Error.t -> 'a decoder
val ( >>= ) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
val transform_result : 'a ok -> 'a decoder
val mapM : ('a -> 'b decoder) -> 'a list -> ('b list) decoder

(** [run core offset dec] starts to decode the source [core] at the position [offset]
    by using [dec] and returns the resulting value. *)
val run : common_source_core -> offset -> 'a decoder -> 'a ok

(** Returns the current position. *)
val current : offset decoder

(** [seek offset] jumps to the position [offset]. *)
val seek : offset -> unit decoder

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

(** Reads 4 bytes as an unsigned integer.
    Due to the limitation of the range representable by the type [int] under 32-bit environments,
    this function returns [wint]-typed values. *)
val d_uint32 : wint decoder

(** Same as [d_uint32], except that return values are of type [int].
    Strictly speaking, this function is not valid under 32-bit environments. *)
val d_uint32_int : int decoder

(** Reads 4 bytes as a Unicode code point. *)
val d_code_point : Uchar.t decoder

(** [pick offset dec] reads data at [offset] by using [dec], and does NOT move the position. *)
val pick : offset -> 'a decoder -> 'a decoder

(** [d_fetch_long origin dec] reads a 4-byte relative offset [rel],
    fetches the value at the position [(origin + rel)] by using [dec],
    and returns the pair of the position and the value. *)
val d_fetch_long : offset -> 'a decoder -> (offset * 'a) decoder

(** [d_repeat count dec] decodes a sequence of values of length [count] by using [dec]. *)
val d_repeat : int -> 'a decoder -> ('a list) decoder

(** [d_list dec] reads a 2-byte unsigned integer [count] and then behaves the same way as [d_repeat count dec]. *)
val d_list : 'a decoder -> ('a list) decoder

(** Reads a 4cc tag. *)
val d_tag : Value.Tag.t decoder

type format_version_result =
  | InitTtf
  | InitCff
  | InitCollection

(** Reads the initial 4 bytes of an entire font file. *)
val d_format_version : format_version_result decoder

(** Reads the list of offsets in the TTC header. *)
val d_ttc_header_offset_list : (offset list) decoder

(** Reads the table directory. *)
val d_structure : table_directory decoder

(** Given a source [common] and a table tag [tag], [seek_required_table common tag] returns
    the pair of the offset and the length of the table. *)
val seek_required_table : common_source -> Value.Tag.t -> (offset * int) ok
