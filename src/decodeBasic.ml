(*  `DecodeBasic` handles entries that can be seen from
    any place within `Otfed.Decode`,
    and is included by `Otfed.Decode`. *)

open Basic


module Error = DecodeError

type 'a ok = ('a, Error.t) result

type common_source_core = {
  data  : string;
  max   : offset;
}

module TableDirectory = Map.Make(Value.Tag)

type table_directory = (offset * int) TableDirectory.t

type common_source = {
  core            : common_source_core;
  table_directory : table_directory;
}

type ttf_source = {
  ttf_common : common_source;
}

type cff_source = {
  cff_common : common_source;
}

type specific_source =
  | Ttf of ttf_source
  | Cff of cff_source

type source = common_source * specific_source

type single_or_collection =
  | Single     of source
  | Collection of source list

type cmap_segment =
  | Incremental of Uchar.t * Uchar.t * Value.glyph_id
  | Constant    of Uchar.t * Uchar.t * Value.glyph_id
