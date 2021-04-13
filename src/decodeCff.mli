
open Basic
open Value
open DecodeBasic
open DecodeOperation.Open

module Maxp : (module type of DecodeCffMaxp)

val fetch_cff_first : common_source_core -> table_directory -> cff_first ok

type charstring_constant = {
  gsubr_index : subroutine_index;
  lsubr_index : subroutine_index;
}

type charstring_state

val d_charstring : charstring_constant -> charstring_state -> (charstring_state * charstring_operation Alist.t) decoder

val charstring : cff_source -> glyph_id -> ((int option * charstring) option) ok

val initial_charstring_state : int -> charstring_state

val path_of_charstring : charstring -> (cubic_path list) ok
