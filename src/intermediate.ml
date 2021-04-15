
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
    | GlyphLocation of int

  type flag = {
    on_curve       : bool;
    x_short_vector : bool;
    y_short_vector : bool;
    this_x_is_same : bool;
    this_y_is_same : bool;
  }
  [@@deriving show { with_path = false }]

  type component_flag = {
    arg_1_and_2_are_words    : bool;
    args_are_xy_values       : bool;
    round_xy_to_grid         : bool;
    we_have_a_scale          : bool;
    we_have_an_x_and_y_scale : bool;
    we_have_a_two_by_two     : bool;
    we_have_instructions     : bool;
    use_my_metrics           : bool;
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

  type cs_x = int
  [@@deriving show { with_path = false }]

  type cs_y = int
  [@@deriving show { with_path = false }]

  type cs_point = cs_x * cs_y
  [@@deriving show { with_path = false }]

  type charstring_operation =
    | HStem     of int * int * cs_point list                                             (* `hstem (1)` *)
    | VStem     of int * int * cs_point list                                             (* `vstem (3)` *)
    | VMoveTo   of int                                                                   (* `vmoveto (4)` *)
    | RLineTo   of cs_point list                                                         (* `rlineto (5)` *)
    | HLineTo   of int list                                                              (* `hlineto (6)` *)
    | VLineTo   of int list                                                              (* `vlineto (7)` *)
    | RRCurveTo of (cs_point * cs_point * cs_point) list                                 (* `rrcurveto (8)` *)
    | HStemHM   of int * int * cs_point list                                             (* `hstemhm (18)` *)
    | HintMask  of stem_argument                                                         (* `hintmask (19)` *)
    | CntrMask  of stem_argument                                                         (* `cntrmask (20)` *)
    | RMoveTo   of cs_point                                                              (* `rmoveto (21)` *)
    | HMoveTo   of int                                                                   (* `hmoveto (22)` *)
    | VStemHM   of int * int * cs_point list                                             (* `vstemhm (23)` *)
    | VVCurveTo of cs_x option * (cs_y * cs_point * cs_y) list                           (* `vvcurveto (26)` *)
    | HHCurveTo of cs_y option * (cs_x * cs_point * cs_x) list                           (* `hhcurveto (27)` *)
    | VHCurveTo of (int * cs_point * int) list * int option                              (* `vhcurveto (30)` *)
    | HVCurveTo of (int * cs_point * int) list * int option                              (* `hvcurveto (31)` *)
    | Flex      of cs_point * cs_point * cs_point * cs_point * cs_point * cs_point * int (* `flex (12 35)` *)
    | HFlex     of int * cs_point * int * int * int * int                                (* `hflex (12 34)` *)
    | HFlex1    of cs_point * cs_point * int * int * cs_point * int                      (* `hflex1 (12 36)` *)
    | Flex1     of cs_point * cs_point * cs_point * cs_point * cs_point * int            (* `flex1 (12 37)` *)
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

  type top_dict = {
    font_name           : string;
    is_fixed_pitch      : bool;
    italic_angle        : int;
    underline_position  : int;
    underline_thickness : int;
    paint_type          : int;
    font_bbox           : int * int * int * int;
    stroke_width        : int;
    cid_info            : cid_info option;
    number_of_glyphs    : int;
  }
end
