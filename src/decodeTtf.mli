
open Value
open DecodeBasic
open DecodeOperation.Open

module Maxp : (module type of DecodeTtfMaxp)

val loca : ttf_source -> glyph_id -> (ttf_glyph_location option) ok

val d_glyf : ttf_glyph_info decoder

val glyf : ttf_source -> ttf_glyph_location -> ttf_glyph_info ok

val path_of_ttf_contour : ttf_contour -> quadratic_path ok
