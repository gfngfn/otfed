
open Basic
open DecodeOperation.Open
open Intermediate.Cff

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
