
open Basic
open Value
open DecodeBasic

include module type of DecodeOperationCore

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

val d_loc_format : loc_format decoder

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
