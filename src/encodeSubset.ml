
open Value


let make_ttf_subset (_ttf : DecodeBasic.ttf_source) (_gids : glyph_id list) =
  failwith "Encode.Subset.make_ttf"


let make_cff_subset (_cff : DecodeBasic.cff_source) (_gids : glyph_id list) =
  failwith "Encode.Subset.make_cff"


let make (src : DecodeBasic.source) (gids : glyph_id list) =
  match src with
  | Ttf(ttf) -> make_ttf_subset ttf gids
  | Cff(cff) -> make_cff_subset cff gids
