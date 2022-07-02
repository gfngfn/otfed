
open Value
open DecodeBasic
open DecodeOperation.Open

module Maxp : (module type of DecodeTtfMaxp)

val loca : ttf_source -> glyph_id -> (Intermediate.Ttf.glyph_location option) ok

val d_glyph : Ttf.glyph_info decoder

val glyf : ttf_source -> Intermediate.Ttf.glyph_location -> Ttf.glyph_info ok

val path_of_contour : Ttf.contour -> quadratic_path ok
