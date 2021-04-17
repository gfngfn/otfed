
open Basic
open DecodeBasic
open DecodeOperation.Open

module Maxp : (module type of DecodeCffMaxp)

val fetch_cff_specific : common_source_core -> table_directory -> cff_specific_source ok

val top_dict : cff_source -> Intermediate.Cff.top_dict ok

type charstring_constant = {
  gsubr_index : subroutine_index;
  lsubr_index : subroutine_index;
}

type charstring_state

val d_charstring : charstring_constant -> charstring_state -> (charstring_state * Intermediate.Cff.charstring_operation Alist.t) decoder

val charstring : cff_source -> Value.glyph_id -> ((int option * Intermediate.Cff.charstring) option) ok

val initial_charstring_state : int -> charstring_state

module LexicalSubroutineIndex : sig
  type t
  val empty : t
  val add : int -> Intermediate.Cff.lexical_charstring -> t -> t
  val mem : int -> t -> bool
  val find : int -> t -> Intermediate.Cff.lexical_charstring option
end

val fdindex : cff_source -> Value.glyph_id -> (fdindex option) ok

val lexical_charstring : cff_source -> gsubrs:LexicalSubroutineIndex.t -> lsubrs:LexicalSubroutineIndex.t -> Value.glyph_id -> ((LexicalSubroutineIndex.t * LexicalSubroutineIndex.t * Intermediate.Cff.lexical_charstring) option) ok

val path_of_charstring : Intermediate.Cff.charstring -> (Value.cubic_path list) ok
