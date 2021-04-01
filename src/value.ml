
open Basic


module Tag : sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_wide_int : wint -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val format_version_OTTO : t
  val format_version_true : t
  val format_version_1_0 : t
  val format_version_ttcf : t

  val table_cmap : t
  val table_head : t
  val table_hhea : t
  val table_hmtx : t
  val table_maxp : t
  val table_name : t
  val table_os2  : t
  val table_post : t

  val table_cvt  : t
  val table_fpgm : t
  val table_glyf : t
  val table_loca : t
  val table_prep : t

  val table_cff  : t
  val table_vorg : t

  val table_base : t
  val table_gdef : t
  val table_gpos : t
  val table_gsub : t
  val table_jstf : t
  val table_math : t

  val table_kern : t

end = struct
  type t = wint

  let equal = WideInt.equal

  let compare = WideInt.compare

  let of_wide_int n = n

  let to_string tag =
    let ui = tag in
    let open WideInt in
    let b0 = ui lsr 24 in
    let r0 = ui -% (b0 lsl 24) in
    let b1 = r0 lsr 16 in
    let r1 = r0 -% (b1 lsl 16) in
    let b2 = r1 lsr 8 in
    let b3 = r1 -% (b2 lsl 8) in
    Printf.sprintf "%c%c%c%c"
      (to_byte b0)
      (to_byte b1)
      (to_byte b2)
      (to_byte b3)

  let pp ppf tag =
    Format.fprintf ppf "%s" (to_string tag)

  let format_version_OTTO = !%% 0x4F54544FL
  let format_version_true = !%% 0x74727565L
  let format_version_1_0  = !%% 0x00010000L
  let format_version_ttcf = !%% 0x74746366L

  let table_cmap = !%% 0x636D6170L
  let table_head = !%% 0x68656164L
  let table_hhea = !%% 0x68686561L
  let table_hmtx = !%% 0x686D7478L
  let table_maxp = !%% 0x6D617870L
  let table_name = !%% 0x6E616D65L
  let table_os2  = !%% 0x4F532F32L
  let table_post = !%% 0x706F7374L

  let table_cvt  = !%% 0x63767420L
  let table_fpgm = !%% 0x6670676DL
  let table_glyf = !%% 0x676C7966L
  let table_loca = !%% 0x6C6F6361L
  let table_prep = !%% 0x70726570L

  let table_cff  = !%% 0x43464620L
  let table_vorg = !%% 0x564F5247L

  let table_base = !%% 0x42415345L
  let table_gdef = !%% 0x47444546L
  let table_gpos = !%% 0x47504F53L
  let table_gsub = !%% 0x47535542L
  let table_jstf = !%% 0x4A535446L
  let table_math = !%% 0x4d415448L

  let table_kern = !%% 0x6B65726EL

end

type glyph_id = int
[@@deriving show {with_path = false}]

type timestamp = wint
[@@deriving show {with_path = false}]

type x_coordinate = int
[@@deriving show { with_path = false }]

type y_coordinate = int
[@@deriving show { with_path = false }]

type point = x_coordinate * y_coordinate
[@@deriving show { with_path = false }]

type ttf_contour = (bool * x_coordinate * y_coordinate) list
[@@deriving show { with_path = false }]

type linear_transform = {
  a : float;
  b : float;
  c : float;
  d : float;
}
[@@deriving show { with_path = false }]

type composition =
  | Vector   of x_coordinate * y_coordinate
  | Matching of int * int
[@@deriving show { with_path = false }]

type ttf_simple_glyph_description = ttf_contour list
[@@deriving show { with_path = false }]

type ttf_composite_glyph_description = (glyph_id * composition * linear_transform option) list
[@@deriving show { with_path = false }]

type ttf_glyph_description =
  | TtfSimpleGlyph    of ttf_simple_glyph_description
  | TtfCompositeGlyph of ttf_composite_glyph_description
[@@deriving show { with_path = false }]

type bounding_box = {
  x_min : x_coordinate;
  y_min : y_coordinate;
  x_max : x_coordinate;
  y_max : y_coordinate;
}
[@@deriving show { with_path = false }]

type cubic_path_element =
  | CubicLineTo  of point
  | CubicCurveTo of point * point * point
[@@deriving show { with_path = false }]

type cubic_path = point * cubic_path_element list
[@@deriving show { with_path = false }]

type quadratic_path_element =
  | QuadraticLineTo  of point
  | QuadraticCurveTo of point * point
[@@deriving show { with_path = false }]

type quadratic_path = point * quadratic_path_element list
[@@deriving show { with_path = false }]

module Cmap = struct
  type t

  type subtable

  type subtable_ids = {
    platform_id : int;
    encoding_id : int;
    format      : int;
  }
end

module Head = struct
  type t = {
    font_revision       : wint;
    flags               : int;
    units_per_em        : int;
    created             : timestamp;
    modified            : timestamp;
    mac_style           : int;
    lowest_rec_ppem     : int;
  }
  [@@deriving show {with_path = false}]
end

module Hhea = struct
  type t = {
    ascender               : int;
    descender              : int;
    line_gap               : int;
    advance_width_max      : int;
    min_left_side_bearing  : int;
    min_right_side_bearing : int;
    xmax_extent            : int;
    caret_slope_rise       : int;
    caret_slope_run        : int;
    caret_offset           : int;
  }
  [@@deriving show {with_path = false}]
end

module Os2 = struct
  type t = {
    x_avg_char_width            : int;
    us_weight_class             : int;
    us_width_class              : int;
    fs_type                     : int;
    y_subscript_x_size          : int;
    y_subscript_y_size          : int;
    y_subscript_x_offset        : int;
    y_subscript_y_offset        : int;
    y_superscript_x_size        : int;
    y_superscript_y_size        : int;
    y_superscript_x_offset      : int;
    y_superscript_y_offset      : int;
    y_strikeout_size            : int;
    y_strikeout_position        : int;
    s_family_class              : int;
    panose                      : string;  (* 10 bytes. *)
    ul_unicode_range1           : wint;
    ul_unicode_range2           : wint;
    ul_unicode_range3           : wint;
    ul_unicode_range4           : wint;
    ach_vend_id                 : string;  (* 4 bytes. *)
    fs_selection                : int;
    us_first_char_index         : int;
    us_last_char_index          : int;
    s_typo_ascender             : int;
    s_type_descender            : int;
    s_typo_linegap              : int;
    us_win_ascent               : int;
    us_win_descent              : int;
    ul_code_page_range_1        : wint option;
    ul_code_page_range_2        : wint option;
    s_x_height                  : int option;
    s_cap_height                : int option;
    us_default_char             : int option;
    us_break_char               : int option;
    us_max_context              : int option;
    us_lower_optical_point_size : int option;
    us_upper_optical_point_size : int option;
  }
  [@@deriving show {with_path = false}]
end

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
  [@@deriving show {with_path = false}]
end

module Math = struct
  type device_table = int * int * int * int
  [@@deriving show {with_path = false}]

  type math_value_record = int * device_table option
  [@@deriving show {with_path = false}]

  type math_constants = {
    script_percent_scale_down                     : int;
    script_script_percent_scale_down              : int;
    delimited_sub_formula_min_height              : int;
    display_operator_min_height                   : int;
    math_leading                                  : math_value_record;
    axis_height                                   : math_value_record;
    accent_base_height                            : math_value_record;
    flattened_accent_base_height                  : math_value_record;
    subscript_shift_down                          : math_value_record;
    subscript_top_max                             : math_value_record;
    subscript_baseline_drop_min                   : math_value_record;
    superscript_shift_up                          : math_value_record;
    superscript_shift_up_cramped                  : math_value_record;
    superscript_bottom_min                        : math_value_record;
    superscript_baseline_drop_max                 : math_value_record;
    sub_superscript_gap_min                       : math_value_record;
    superscript_bottom_max_with_subscript         : math_value_record;
    space_after_script                            : math_value_record;
    upper_limit_gap_min                           : math_value_record;
    upper_limit_baseline_rise_min                 : math_value_record;
    lower_limit_gap_min                           : math_value_record;
    lower_limit_baseline_drop_min                 : math_value_record;
    stack_top_shift_up                            : math_value_record;
    stack_top_display_style_shift_up              : math_value_record;
    stack_bottom_shift_down                       : math_value_record;
    stack_bottom_display_style_shift_down         : math_value_record;
    stack_gap_min                                 : math_value_record;
    stack_display_style_gap_min                   : math_value_record;
    stretch_stack_top_shift_up                    : math_value_record;
    stretch_stack_bottom_shift_down               : math_value_record;
    stretch_stack_gap_above_min                   : math_value_record;
    stretch_stack_gap_below_min                   : math_value_record;
    fraction_numerator_shift_up                   : math_value_record;
    fraction_numerator_display_style_shift_up     : math_value_record;
    fraction_denominator_shift_down               : math_value_record;
    fraction_denominator_display_style_shift_down : math_value_record;
    fraction_numerator_gap_min                    : math_value_record;
    fraction_num_display_style_gap_min            : math_value_record;
    fraction_rule_thickness                       : math_value_record;
    fraction_denominator_gap_min                  : math_value_record;
    fraction_denom_display_style_gap_min          : math_value_record;
    skewed_fraction_horizontal_gap                : math_value_record;
    skewed_fraction_vertical_gap                  : math_value_record;
    overbar_vertical_gap                          : math_value_record;
    overbar_rule_thickness                        : math_value_record;
    overbar_extra_ascender                        : math_value_record;
    underbar_vertical_gap                         : math_value_record;
    underbar_rule_thickness                       : math_value_record;
    underbar_extra_descender                      : math_value_record;
    radical_vertical_gap                          : math_value_record;
    radical_display_style_vertical_gap            : math_value_record;
    radical_rule_thickness                        : math_value_record;
    radical_extra_ascender                        : math_value_record;
    radical_kern_before_degree                    : math_value_record;
    radical_kern_after_degree                     : math_value_record;
    radical_degree_bottom_raise_percent           : int;
  }
  [@@deriving show {with_path = false}]

  type math_kern = math_value_record list * math_value_record list
  [@@deriving show {with_path = false}]

  type math_kern_info_record = {
    top_right_math_kern    : math_kern option;
    top_left_math_kern     : math_kern option;
    bottom_right_math_kern : math_kern option;
    bottom_left_math_kern  : math_kern option;
  }
  [@@deriving show {with_path = false}]

  type math_glyph_info = {
    math_italics_correction    : (glyph_id * math_value_record) list;
    math_top_accent_attachment : (glyph_id * math_value_record) list;
    math_kern_info             : (glyph_id * math_kern_info_record) list;
  }
  [@@deriving show {with_path = false}]

  type glyph_part_record = {
    glyph_id_for_part      : glyph_id;
    start_connector_length : int;
    end_connector_length   : int;
    full_advance           : int;
    part_flags             : int;
  }
  [@@deriving show {with_path = false}]

  type math_glyph_construction = {
    glyph_assembly                 : (math_value_record * glyph_part_record list) option;
    math_glyph_variant_record_list : (glyph_id * int) list;
  }
  [@@deriving show {with_path = false}]

  type math_variants = {
    min_connector_overlap : int;
    vert_glyph_assoc      : (glyph_id * math_glyph_construction) list;
    horiz_glyph_assoc     : (glyph_id * math_glyph_construction) list;
  }
  [@@deriving show {with_path = false}]

  type t = {
    math_constants  : math_constants;
    math_glyph_info : math_glyph_info;
    math_variants   : math_variants;
  }
  [@@deriving show {with_path = false}]
end
