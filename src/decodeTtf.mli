
open Value
open DecodeBasic
open DecodeOperation.Open

module Maxp : (module type of DecodeTtfMaxp)

val d_loca : num_glyphs:int -> Intermediate.loc_format -> glyph_id -> (((int * int) option) option) decoder

val loca : ttf_source -> glyph_id -> (Intermediate.Ttf.loca_entry option) ok

val d_glyph : length:int -> Ttf.glyph_info decoder

val glyf : ttf_source -> Intermediate.Ttf.glyph_location -> Ttf.glyph_info ok

val path_of_contour : Ttf.contour -> quadratic_path ok
