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

type ttf_source = {
  ttf_common : common_source;
}

type cff_source = {
  cff_common : common_source;
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

type cmap_segment =
  | Incremental of Uchar.t * Uchar.t * Value.glyph_id
  | Constant    of Uchar.t * Uchar.t * Value.glyph_id

type offsize = OffSize1 | OffSize2 | OffSize3 | OffSize4

type cff_key =
  | ShortKey of int
  | LongKey  of int
[@@deriving show { with_path = false }]

type cff_value =
  | Integer of int
  | Real    of float

type dict_element =
  | Value of cff_value
  | Key   of cff_key

module DictMap = Map.Make
  (struct
    type t = cff_key
    let compare kt1 kt2 =
      match (kt1, kt2) with
      | (ShortKey(i1), ShortKey(i2)) -> Int.compare i1 i2
      | (ShortKey(_), LongKey(_))    -> -1
      | (LongKey(_), ShortKey(_))    -> 1
      | (LongKey(i1), LongKey(i2))   -> Int.compare i1 i2
  end)

(* The type for DICT data [CFF p.9, Section 4] *)
type dict = (cff_value list) DictMap.t

(* The type for String INDEXes [CFF p.17, Section 10] *)
type string_index = string array

(* Represents a bit vector of arbitrary finite length *)
type stem_argument = string
[@@deriving show { with_path = false }]

type charstring_element =
  | ArgInteger  of int
  | ArgReal     of float

  | OpHStem   (* `hstem (1)` *)
  | OpVStem   (* `vstem (3)` *)
  | OpHStemHM (* `hstemhm (18)` *)
  | OpVStemHM (* `hstemhm (23)` *)

  | OpRMoveTo (* `rmoveto (21)` *)
  | OpHMoveTo (* `hmoveto (22)` *)
  | OpVMoveTo   (* `vmoveto (4)` *)

  | OpRLineTo   (* `rlineto (5)` *)
  | OpHLineTo   (* `hlineto (6)` *)
  | OpVLineTo   (* `vlineto (7)` *)

  | OpCallSubr  (* `callsubr (10)` *)
  | OpCallGSubr (* `callgsubr (29)` *)

  | OpReturn  (* `return (11)` *)
  | OpEndChar (* `endchar (14)` *)

  | OpHintMask  of stem_argument (* `hintmask (19)` *)
  | OpCntrMask  of stem_argument (* `cntrmask (20)` *)

  | OpRCurveLine (* `rcurveline (24)` *)
  | OpRLineCurve (* `rlinecurve (25)` *)
  | OpRRCurveTo  (* `rrcurveto (8)` *)
  | OpVVCurveTo  (* `vvcurveto (26)` *)
  | OpHHCurveTo  (* `hhcurveto (27)` *)
  | OpVHCurveTo  (* `vhcurveto (30)` *)
  | OpHVCurveTo  (* `hvcurveto (31)` *)

  | OpHFlex  (* `hflex (12 34)` *)
  | OpFlex   (* `flex (12 35)` *)
  | OpHFlex1 (* `hflex1 (12 36)` *)
  | OpFlex1  (* `flex1 (12 37)` *)
[@@deriving show { with_path = false }]

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
  offSize  : offsize;
}

type cff_first = {
  cff_header   : cff_header;
  cff_name     : string;           (* singleton Name INDEX *)
  top_dict     : dict;             (* singleton Top DICT INDEX *)
  string_index : string_index;     (* String INDEX [CFF p.17, Section 10] *)
  gsubr_index  : subroutine_index;
  offset_CFF   : int;
}

(* The type for CIDFont-specific data in Top DICT [CFF p.16, Table 10] *)
type cff_cid_info = {
  registry          : string;
  ordering          : string;
  supplement        : int;
  cid_font_version  : float;
  cid_font_revision : int;
  cid_font_type     : int;
  cid_count         : int;
}

type cff_top_dict = {
  font_name           : string;
  is_fixed_pitch      : bool;
  italic_angle        : int;
  underline_position  : int;
  underline_thickness : int;
  paint_type          : int;
  font_bbox           : int * int * int * int;
  stroke_width        : int;
  cid_info            : cff_cid_info option;
  number_of_glyphs    : int;
}

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

type charstring_info = subroutine_index * private_info * int

type cs_x = int
[@@deriving show { with_path = false }]

type cs_y = int
[@@deriving show { with_path = false }]

type cs_point = cs_x * cs_y
[@@deriving show { with_path = false }]

type charstring_operation =
  | HStem     of int * int * cs_point list                                             (* [hstem (1)] *)
  | VStem     of int * int * cs_point list                                             (* [vstem (3)] *)
  | VMoveTo   of int                                                                   (* [vmoveto (4)] *)
  | RLineTo   of cs_point list                                                         (* [rlineto (5)] *)
  | HLineTo   of int list                                                              (* [hlineto (6)] *)
  | VLineTo   of int list                                                              (* [vlineto (7)] *)
  | RRCurveTo of (cs_point * cs_point * cs_point) list                                 (* [rrcurveto (8)] *)
  | HStemHM   of int * int * cs_point list                                             (* [hstemhm (18)] *)
  | HintMask  of stem_argument                                                         (* [hintmask (19)] *)
  | CntrMask  of stem_argument                                                         (* [cntrmask (20)] *)
  | RMoveTo   of cs_point                                                              (* [rmoveto (21)] *)
  | HMoveTo   of int                                                                   (* [hmoveto (22)] *)
  | VStemHM   of int * int * cs_point list                                             (* [vstemhm (23)] *)
  | VVCurveTo of cs_x option * (cs_y * cs_point * cs_y) list                           (* [vvcurveto (26)] *)
  | HHCurveTo of cs_y option * (cs_x * cs_point * cs_x) list                           (* [hhcurveto (27)] *)
  | VHCurveTo of (int * cs_point * int) list * int option                              (* [vhcurveto (30)] *)
  | HVCurveTo of (int * cs_point * int) list * int option                              (* [hvcurveto (31)] *)
  | Flex      of cs_point * cs_point * cs_point * cs_point * cs_point * cs_point * int (* [flex (12 35)] *)
  | HFlex     of int * cs_point * int * int * int * int                                (* [hflex (12 34)] *)
  | HFlex1    of cs_point * cs_point * int * int * cs_point * int                      (* [hflex1 (12 36)] *)
  | Flex1     of cs_point * cs_point * cs_point * cs_point * cs_point * int            (* [flex1 (12 37)] *)
[@@deriving show { with_path = false }]

type charstring = charstring_operation list
[@@deriving show { with_path = false }]

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
