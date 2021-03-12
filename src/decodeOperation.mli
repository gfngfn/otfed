
open Basic
open Value
open DecodeBasic

include module type of DecodeOperationCore

(** Same as [d_uint32], except that return values are of type [int].
    Strictly speaking, this function is not valid under 32-bit environments. *)
val d_uint32_int : int decoder

(** Reads 4 bytes as a Unicode code point. *)
val d_code_point : Uchar.t decoder

val d_f2dot14 : float decoder

(** [pick offset dec] reads data at [offset] by using [dec], and does NOT move the position. *)
val pick : offset -> 'a decoder -> 'a decoder

(** [d_offset origin] reads 2 bytes as a relative offset [rel], and returns [origin + rel]. *)
val d_offset : offset -> offset decoder

(** Same as [d_offset] except that [d_offset_opt] returns [None] when it has read [0]. *)
val d_offset_opt : offset -> (offset option) decoder

(** [d_fetch origin dec] reads 2 bytes as a relative offset [rel]
    and then reads data at [origin + rel] by using [dec].
    Here, the position advances by 2 bytes. *)
val d_fetch : offset -> 'a decoder -> 'a decoder

(** [d_fetch_long origin dec] reads a 4-byte relative offset [rel],
    fetches the value at the position [(origin + rel)] by using [dec],
    and returns the pair of the position and the value. *)
val d_fetch_long : offset -> 'a decoder -> (offset * 'a) decoder

(** [d_repeat count dec] decodes a sequence of values of length [count] by using [dec]. *)
val d_repeat : int -> 'a decoder -> ('a list) decoder

(** [d_list dec] reads a 2-byte unsigned integer [count] and then behaves the same way as [d_repeat count dec]. *)
val d_list : 'a decoder -> ('a list) decoder

(** Reads a 4cc tag. *)
val d_tag : Tag.t decoder

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

(** Reads a Coverage table [page 139]. *)
val d_coverage : (glyph_id list) decoder

val combine_coverage : (glyph_id list) -> 'a list -> ((glyph_id * 'a) list) decoder

(** Reads an OffSize value. *)
val d_offsize : offsize decoder

(** Reads an offset in CFF tables based on the given OffSize. *)
val d_cff_offset : offsize -> wint decoder

val d_twoscompl2 : int decoder

val d_twoscompl4 : int decoder

val d_cff_real : (int * float) decoder

val d_index : (int -> 'a decoder) -> ('a list) decoder

val d_index_singleton : (int -> 'a decoder) -> 'a decoder

val d_index_access : (int -> 'a decoder) -> int -> ('a option) decoder

val d_dict : int -> dict decoder

(** Given a table directory [td] and a table tag [tag], [seek_required_table td tag] returns
    the pair of the offset and the length of the table. *)
val seek_required_table : table_directory -> Tag.t -> (offset * int) ok

val seek_table : table_directory -> Tag.t -> (offset * int) option
