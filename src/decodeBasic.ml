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

(* Entries `(tag ↦ (offset, length))` contained in a mapping of type `table_directory` stands for:
   - `offset`: where the table tagged as `tag` starts, and
   - `length`: how long the table is. *)
type table_directory = (offset * int) TableDirectory.t

type common_source = {
  core            : common_source_core;
  table_directory : table_directory;
  loc_format      : Intermediate.loc_format;
  num_glyphs      : int;
  num_h_metrics   : int;
}

type cmap_segment =
  | Incremental of Uchar.t * Uchar.t * Value.glyph_id
  | Constant    of Uchar.t * Uchar.t * Value.glyph_id

(* The type for String INDEXes [CFF p.17, Section 10] *)
type string_index = string array

(* `CharString(offset, length)`
   ` `offset`: the (absolute) offset to the charstring data
   - `length`: the length of the charstring data *)
type charstring_data =
  | CharStringData of int * int

(* The type for Local/Global Subrs INDEXes [CFF p.25, Section 16] *)
type subroutine_index = charstring_data array

(* The type for CFF headers [CFF p.13, Section 6] *)
type cff_header = {
  major    : int;
  minor    : int;
  hdrSize  : int;
  offSize  : Intermediate.Cff.offsize;
}
[@@deriving show { with_path = false }]

(* The type for Private DICT [CFF p.23, Section 15] *)
type single_private = {
  default_width_x  : int;
  nominal_width_x  : int;
  local_subr_index : subroutine_index;
}

type fdarray = single_private array

type fdindex = int

(* The type for FDSelect [CFF p.28, Section 19] *)
type fdselect =
  | FDSelectFormat0 of fdindex array
  | FDSelectFormat3 of (Value.glyph_id * fdindex) list * Value.glyph_id

type private_info =
  | SinglePrivate of single_private
  | FontDicts     of fdarray * fdselect

type charstring_info = {
  gsubr_index             : subroutine_index;
  private_info            : private_info;
  offset_CharString_INDEX : offset;
}

type cff_specific_source = {
  cff_top_dict    : Intermediate.Cff.top_dict;
  charstring_info : charstring_info;
}

type ttf_source = {
  ttf_common : common_source;
}

type cff_source = {
  cff_common   : common_source;
  cff_specific : cff_specific_source;
}

type source =
  | Ttf of ttf_source
  | Cff of cff_source


let get_common_source = function
  | Ttf(ttf) -> ttf.ttf_common
  | Cff(cff) -> cff.cff_common


type single_or_collection =
  | Single     of source
  | Collection of source list


module GeneralTable(Info : sig type t end) = struct

  type t = {
    core   : common_source_core;
    offset : offset;
    length : int;
    info   : Info.t;
  }


  let make_scheme (core : common_source_core) (offset : offset) (length : int) (info : Info.t) : t =
    {
      core   = core;
      offset = offset;
      length = length;
      info   = info;
    }


  let get_info (general_table : t) : Info.t =
    general_table.info

end
