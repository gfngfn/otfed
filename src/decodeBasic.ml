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

type cff_specific_source = {
  cff_top_dict    : Intermediate.Cff.top_dict;
  charstring_info : Intermediate.Cff.charstring_info;
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
