
open Basic


type loc_format =
  | ShortLocFormat
  | LongLocFormat
[@@deriving show { with_path = false }]


module Head = struct

  type derived = {
    x_min               : int;
    y_min               : int;
    x_max               : int;
    y_max               : int;
    index_to_loc_format : loc_format;
  }
  [@@deriving show { with_path = false }]

  type t = {
    value   : Value.Head.t;
    derived : derived;
  }
  [@@deriving show { with_path = false }]

end


module Hhea = struct

  type derived = {
    advance_width_max      : int;
    min_left_side_bearing  : int;
    min_right_side_bearing : int;
    xmax_extent            : int;
  }
  [@@deriving show { with_path = false }]

  type t = {
    value   : Value.Hhea.t;
    derived : derived;
  }
  [@@deriving show { with_path = false }]

end


module Os2 = struct
  type derived = {
    x_avg_char_width    : int;
    us_first_char_index : Uchar.t; [@printer pp_uchar]
    us_last_char_index  : Uchar.t; [@printer pp_uchar]
    us_max_context      : int option;
  }
  [@@deriving show { with_path = false }]

  type t = {
    value   : Value.Os2.t;
    derived : derived;
  }
  [@@deriving show { with_path = false }]
end


module Ttf = struct
  module Maxp = struct
    type t = {
      num_glyphs               : int;
      max_points               : int;
      max_contours             : int;
      max_composite_points     : int;
      max_composite_contours   : int;
      max_zones                : int;
      max_twilight_points      : int;
      max_storage              : int;
      max_function_defs        : int;
      max_instruction_defs     : int;
      max_stack_elements       : int;
      max_size_of_instructions : int;
      max_component_elements   : int;
      max_component_depth      : int;
    }
    [@@deriving show { with_path = false }]
  end

  type glyph_location =
    | GlyphLocation of { reloffset : int; length : int }
  [@@deriving show { with_path = false }]

  type flag = {
    on_curve       : bool;
    x_short_vector : bool;
    y_short_vector : bool;
    this_x_is_same : bool;
    this_y_is_same : bool;
  }
  [@@deriving show { with_path = false }]

  type component_flag = {
    arg_1_and_2_are_words     : bool;
    args_are_xy_values        : bool;
    round_xy_to_grid          : bool;
    we_have_a_scale           : bool;
    we_have_an_x_and_y_scale  : bool;
    we_have_a_two_by_two      : bool;
    we_have_instructions      : bool;
    use_my_metrics            : bool;
    unscaled_component_offset : bool;
  }
  [@@deriving show { with_path = false }]
end


module Cff = struct

  module Maxp = struct
    type t = {
      num_glyphs : int;
    }
    [@@deriving show { with_path = false }]
  end

  type offsize = OffSize1 | OffSize2 | OffSize3 | OffSize4
  [@@deriving show { with_path = false }]

  type key =
    | ShortKey of int
    | LongKey  of int
  [@@deriving show { with_path = false }]

  type value =
    | Integer of int
    | Real    of float
  [@@deriving show { with_path = false }]

  type dict_element =
    | Value of value
    | Key   of key
  [@@deriving show { with_path = false }]

  module DictMap = Map.Make
    (struct
      type t = key
      let compare kt1 kt2 =
        match (kt1, kt2) with
        | (ShortKey(i1), ShortKey(i2)) -> Int.compare i1 i2
        | (ShortKey(_), LongKey(_))    -> -1
        | (LongKey(_), ShortKey(_))    -> 1
        | (LongKey(i1), LongKey(i2))   -> Int.compare i1 i2
    end)

  (* The type for DICT data [CFF p.9, Section 4] *)
  type dict = (value list) DictMap.t

  (* Represents a bit vector of arbitrary finite length *)
  type stem_argument = string
  [@@deriving show { with_path = false }]

  type charstring_token =
    | ArgumentInteger  of int
    | ArgumentReal     of float

    | OpHStem   (* `hstem (1)` *)
    | OpVStem   (* `vstem (3)` *)
    | OpHStemHM (* `hstemhm (18)` *)
    | OpVStemHM (* `hstemhm (23)` *)

    | OpRMoveTo (* `rmoveto (21)` *)
    | OpHMoveTo (* `hmoveto (22)` *)
    | OpVMoveTo (* `vmoveto (4)` *)

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

  type lexical_charstring = charstring_token list
  [@@deriving show { with_path = false }]

  type vector = Value.design_units * Value.design_units
  [@@deriving show { with_path = false }]

  type charstring_operation =
    | HStem of {
        y    : Value.design_units;
        dy   : Value.design_units;
        rest : (Value.design_units * Value.design_units) list;
      } (* `hstem (1)` *)
    | VStem of {
        x    : Value.design_units;
        dx   : Value.design_units;
        rest : (Value.design_units * Value.design_units) list;
      } (* `vstem (3)` *)
    | VMoveTo of {
        dy1 : Value.design_units;
      } (* `vmoveto (4)` *)
    | RLineTo of (Value.design_units * Value.design_units) list
        (* `rlineto (5)` *)
    | HLineTo of Value.design_units list
        (* `hlineto (6)` *)
    | VLineTo of Value.design_units list
        (* `vlineto (7)` *)
    | RRCurveTo of (vector * vector * vector) list
        (* `rrcurveto (8)` *)
    | HStemHM of {
        y    : Value.design_units;
        dy   : Value.design_units;
        rest : (Value.design_units * Value.design_units) list;
      } (* `hstemhm (18)` *)
    | HintMask of stem_argument
        (* `hintmask (19)` *)
    | CntrMask of stem_argument
        (* `cntrmask (20)` *)
    | RMoveTo of {
        dv1 : vector;
      } (* `rmoveto (21)` *)
    | HMoveTo of {
        dx1 : Value.design_units;
      } (* `hmoveto (22)` *)
    | VStemHM of {
        x    : Value.design_units;
        dx   : Value.design_units;
        rest : (Value.design_units * Value.design_units) list;
      } (* `vstemhm (23)` *)
    | VVCurveTo of {
        dx1  : Value.design_units option;
        rest : (Value.design_units * vector * Value.design_units) list;
      } (* `vvcurveto (26)` *)
    | HHCurveTo of {
        dy1  : Value.design_units option;
        rest : (Value.design_units * vector * Value.design_units) list;
      } (* `hhcurveto (27)` *)
    | VHCurveTo of {
        main : (Value.design_units * vector * Value.design_units) list;
        df   : Value.design_units option;
      } (* `vhcurveto (30)` *)
    | HVCurveTo of {
        main : (Value.design_units * vector * Value.design_units) list;
        df   : Value.design_units option;
      } (* `hvcurveto (31)` *)
    | Flex of {
        dv1 : vector;
        dv2 : vector;
        dv3 : vector;
        dv4 : vector;
        dv5 : vector;
        dv6 : vector;
        fd  : Value.design_units;
      } (* `flex (12 35)` *)
    | HFlex of {
        dx1 : Value.design_units;
        dv2 : vector;
        dx3 : Value.design_units;
        dx4 : Value.design_units;
        dx5 : Value.design_units;
        dx6 : Value.design_units;
      } (* `hflex (12 34)` *)
    | HFlex1 of {
        dv1 : vector;
        dv2 : vector;
        dx3 : Value.design_units;
        dx4 : Value.design_units;
        dv5 : vector;
        dx6 : Value.design_units;
      } (* `hflex1 (12 36)` *)
    | Flex1 of {
        dv1 : vector;
        dv2 : vector;
        dv3 : vector;
        dv4 : vector;
        dv5 : vector;
        d6  : Value.design_units;
      } (* `flex1 (12 37)` *)
  [@@deriving show { with_path = false }]

  type charstring = charstring_operation list
  [@@deriving show { with_path = false }]

  (* The type for CIDFont-specific data in Top DICT [CFF p.16, Table 10] *)
  type cid_info = {
    registry          : string;
    ordering          : string;
    supplement        : int;
    cid_font_version  : float;
    cid_font_revision : int;
    cid_font_type     : int;
    cid_count         : int;
  }
  [@@deriving show { with_path = false }]

  type top_dict = {
    font_name           : string;
    version             : string option;
    notice              : string option;
    copyright           : string option;
    full_name           : string option;
    family_name         : string option;
    weight              : string option;
    is_fixed_pitch      : bool;
    italic_angle        : int;
    underline_position  : int;
    underline_thickness : int;
    paint_type          : int;
    font_bbox           : Value.bounding_box;
    stroke_width        : int;
    cid_info            : cid_info option;
    number_of_glyphs    : int;
  }
  [@@deriving show { with_path = false }]

  (* The type for String INDEXes [CFF p.17, Section 10] *)
  type string_index = string array

  (* `CharString(offset, length)`
     ` `offset`: the (absolute) offset to the charstring data
     - `length`: the length of the charstring data *)
  type charstring_data =
    | CharStringData of int * int
  [@@deriving show { with_path = false }]

  (* The type for Local/Global Subrs INDEXes [CFF p.25, Section 16] *)
  type subroutine_index = charstring_data array

  (* The type for CFF headers [CFF p.13, Section 6] *)
  type cff_header = {
    major    : int;
    minor    : int;
    hdrSize  : int;
    offSize  : offsize;
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
  [@@deriving show { with_path = false }]

  (* The type for FDSelect [CFF p.28, Section 19] *)
  type fdselect =
    | FDSelectFormat0 of fdindex array
    | FDSelectFormat3 of (Value.glyph_id * fdindex) list * Value.glyph_id

  type private_info =
    | SinglePrivate of single_private
    | FontDicts     of fdarray * fdselect

  type predefined_charset =
    | IsoAdobeCharset
    | ExpertCharset
    | ExpertSubsetCharset

  type charset =
    | PredefinedCharset of predefined_charset
    | CharsetData       of offset

  type charstring_info = {
    gsubr_index             : subroutine_index;
    private_info            : private_info;
    charset                 : charset;
    string_index            : string_index;
    offset_CharString_INDEX : offset;
  }
end
