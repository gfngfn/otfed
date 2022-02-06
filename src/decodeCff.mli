
open Basic
open DecodeBasic
open DecodeOperation.Open

module Maxp : (module type of DecodeCffMaxp)

val fetch_cff_specific : common_source_core -> table_directory -> cff_specific_source ok

val top_dict : cff_source -> Intermediate.Cff.top_dict ok

val access_charset : cff_source -> Value.glyph_id -> (string option) ok

type charstring_constant = {
  gsubr_index : Intermediate.Cff.subroutine_index;
  lsubr_index : Intermediate.Cff.subroutine_index;
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
  val fold : (int -> Intermediate.Cff.lexical_charstring -> 'a -> 'a) -> t -> 'a -> 'a
  val cardinal : t -> int
end

(** Gets the FD index for the given glyph ID.
    Returns nothing for non-CID fonts (i.e. for the case where the font has only one local subroutine). *)
val fdindex : cff_source -> Value.glyph_id -> (Intermediate.Cff.fdindex option) ok

(** [lexical_charstring cff ~gsubrs ~lsubrs gid] returns [Ok(gsubrs1, lsubrs1, lcs)] where:
    - [cff] is the source,
    - [gsubrs] is a map that stores already lexed global subroutines with non-biased indices,
    - [lsubrs] is a map that stores already lexed local subroutines with non-biased indices,
    - [gid] is a glyph ID,
    - [gsubrs1] is a map gained by extending [gsubrs] with new global subroutines used by the glyph of ID [gid],
    - [lsubrs1] is a map gained by extending [lsubrs] with new local subroutines used by the glyph of ID [gid], and
    - [lcs] is the tokenized CharString of the glyph of ID [gid].

    Note that local subroutines differ depending on [gid] if the font is a CID font;
    use [fdindex] to track correct local subroutines for each set of glyphs. *)
val lexical_charstring : cff_source -> gsubrs:LexicalSubroutineIndex.t -> lsubrs:LexicalSubroutineIndex.t -> Value.glyph_id -> ((LexicalSubroutineIndex.t * LexicalSubroutineIndex.t * Intermediate.Cff.lexical_charstring) option) ok

val path_of_charstring : Intermediate.Cff.charstring -> (Value.cubic_path list) ok
