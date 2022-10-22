
include module type of Basic

(** Handles master data (i.e. data that cannot be derivable from other data) for OpenType fonts. *)
module Value : sig

  (** Handles 4cc tags. *)
  module Tag : sig
    type t
    val to_string : t -> string
  end

  type glyph_id = int

  type design_units = int

  (** Represents an absolute X-coordinate.
      The unit of measurement is [units_per_em],
      and thus differs for each font in general. *)
  type x_coordinate = design_units

  (** Represents an absolute Y-coordinate.
      The unit of measurement is [units_per_em],
      and thus differs for each font in general. *)
  type y_coordinate = design_units

  (** Represents an absolute coordinate on the XY-plane.
      The unit of measurement is [units_per_em],
      and thus differs for each font in general. *)
  type point = x_coordinate * y_coordinate

  (** A signed integer representing a datetime in number of seconds since 1904-01-01T00:00:00. *)
  type timestamp = wint

  type bounding_box = {
    x_min : x_coordinate;
    y_min : y_coordinate;
    x_max : x_coordinate;
    y_max : y_coordinate;
  }
  [@@deriving show]

  type linear_transform = {
    a : float;
    b : float;
    c : float;
    d : float;
  }
  [@@deriving show]

  (** See [cubic_path]. *)
  type cubic_path_element =
    | CubicLineTo  of point
    | CubicCurveTo of point * point * point
  [@@deriving show]

  (** Represents a (directed) cubic Bézier curve with absolute coordinates on the XY-plane.
      A value [(pt, elems)] of this type stands for the path
      starting at [pt] and subsequent moves of which can be described by [elems]. *)
  type cubic_path = point * cubic_path_element list
  [@@deriving show]

  type quadratic_path_element =
    | QuadraticLineTo  of point
    | QuadraticCurveTo of point * point
  [@@deriving show]

  (** Represents a (directed) quadratic Bézier curve with absolute coordinates on the XY-plane.
      A value [(pt, elems)] of this type stands for the path
      starting at [pt] and subsequent moves of which can be described by [elems]. *)
  type quadratic_path = point * quadratic_path_element list
  [@@deriving show]

  (** Converts quadratic Bézier curves to cubic ones.
      Although this conversion does not lose precision at all in theory,
      it does owing to the rounding error for design units. *)
  val cubic_path_of_quadratic_path : quadratic_path -> cubic_path

  (** [unite_bounding_boxes bbox1 bbox2] returns the minimum bounding box
      that includes both [bbox1] and [bbox2]. *)
  val unite_bounding_boxes : bounding_box -> bounding_box -> bounding_box

  (** [bounding_box_by_points pt1 pt2] returns the minimum bounding box
      that contains both [pt1] and [pt2]. *)
  val bounding_box_by_points : point -> point -> bounding_box

  (** Returns the bounding box of the given path. *)
  val calculate_bounding_box_of_path : cubic_path -> bounding_box

  (** Calculates the bounding box of the given paths. Returns [None] for the empty list. *)
  val calculate_bounding_box_of_paths : cubic_path list -> bounding_box option

  (** The type for ValueRecords (page 214). *)
  type value_record = {
    x_placement  : design_units option;
    y_placement  : design_units option;
    x_advance    : design_units option;
    y_advance    : design_units option;
    x_pla_device : int option;
    y_pla_device : int option;
    x_adv_device : int option;
    y_adv_device : int option;
  }

  type class_value = int

  (** The Type for Device tables (page 142). *)
  type device = {
    start_size   : int;
    delta_values : int list;
  }

  type anchor_adjustment =
    | NoAnchorAdjustment
    | AnchorPointAdjustment  of int
    | DeviceAnchorAdjustment of device * device

  (** The type for Anchor tables (page 215). *)
  type anchor = design_units * design_units * anchor_adjustment

  type mark_class = int

  (** The type for MarkRecord (page 217). *)
  type mark_record = mark_class * anchor

  (** The type for BaseRecords (page 199). Arrays of this type are indexed by [mark_class].
      UNDOCUMENTED (in OpenType 1.8.3): BaseRecord tables sometimes contain NULL pointers. *)
  type base_record = (anchor option) array

  (** The type for ComponentRecords (page 201). Arrays of this type are indexed by [mark_class]. *)
  type component_record = (anchor option) array

  (** The type for LigatureAttath tables (page 201). *)
  type ligature_attach = component_record list

  (** The type for Mark2Records (page 203). Arrays of this type are indexed by [mark_class]. *)
  type mark2_record = anchor array

  module Cmap : sig
    module Mapping : sig
      type t

      val empty : t

      val find : Uchar.t -> t -> glyph_id option

      val add_single : Uchar.t -> glyph_id -> t -> t

      val add_incremental_range : start:Uchar.t -> last:Uchar.t -> gid:glyph_id -> t -> t

      val add_constant_range : start:Uchar.t -> last:Uchar.t -> gid:glyph_id -> t -> t

      val fold : (Uchar.t -> glyph_id -> 'a -> 'a) -> t -> 'a -> 'a

      val number_of_entries : t -> int
    end

    type subtable_ids = {
      platform_id : int;
      encoding_id : int;
    }

    type subtable = {
      subtable_ids : subtable_ids;
      mapping      : Mapping.t;
    }

    type t

    val subtables : t -> subtable set
  end

  (** Defines types for master data in [head] tables. *)
  module Head : sig
    type mac_style = {
      bold      : bool;
      italic    : bool;
      underline : bool;
      outline   : bool;
      shadow    : bool;
      condensed : bool;
      extended  : bool;
    }
    [@@deriving show]

    type t = {
      font_revision   : wint;
      flags           : int;
      units_per_em    : int;
      created         : timestamp;
      modified        : timestamp;
      mac_style       : mac_style;
      lowest_rec_ppem : int;
    }
    [@@deriving show]
  end

  (** Defines types for master data in [hhea] tables. *)
  module Hhea : sig
    type t = {
      ascender         : int;
      descender        : int;
      line_gap         : int;
      caret_slope_rise : int;
      caret_slope_run  : int;
      caret_offset     : int;
    }
    [@@deriving show]
  end

  (** Defines types for master data in [OS/2] tables. *)
  module Os2 : sig
    type weight_class =
      | WeightThin
      | WeightExtraLight
      | WeightLight
      | WeightNormal
      | WeightMedium
      | WeightSemiBold
      | WeightBold
      | WeightExtraBold
      | WeightBlack
    [@@deriving show]

    type width_class =
      | WidthUltraCondensed
      | WidthExtraCondensed
      | WidthCondensed
      | WidthSemiCondensed
      | WidthMedium
      | WidthSemiExpanded
      | WidthExpanded
      | WidthExtraExpanded
      | WidthUltraExpanded
    [@@deriving show]

    type fs_type = {
      restricted_license_embedding : bool;
      preview_and_print_embedding  : bool;
      editable_embedding           : bool;
      no_subsetting                : bool;
      bitmap_embedding_only        : bool;
    }
    [@@deriving show]

    type fs_selection = {
      italic           : bool;
      underscore       : bool;
      negative         : bool;
      outlined         : bool;
      strikeout        : bool;
      bold             : bool;
      regular          : bool;
      use_typo_metrics : bool;
      wws              : bool;
      oblique          : bool;
    }
    [@@deriving show]

    type t = {
      us_weight_class             : weight_class;
      us_width_class              : width_class;
      fs_type                     : fs_type;
      y_subscript_x_size          : design_units;
      y_subscript_y_size          : design_units;
      y_subscript_x_offset        : design_units;
      y_subscript_y_offset        : design_units;
      y_superscript_x_size        : design_units;
      y_superscript_y_size        : design_units;
      y_superscript_x_offset      : design_units;
      y_superscript_y_offset      : design_units;
      y_strikeout_size            : design_units;
      y_strikeout_position        : design_units;
      s_family_class              : int;
      panose                      : string;  (* 10 bytes. *)
      ul_unicode_range1           : wint;
      ul_unicode_range2           : wint;
      ul_unicode_range3           : wint;
      ul_unicode_range4           : wint;
      ach_vend_id                 : string;  (* 4 bytes. *)
      fs_selection                : fs_selection;
      s_typo_ascender             : design_units;
      s_typo_descender            : design_units;
      s_typo_linegap              : design_units;
      us_win_ascent               : design_units;
      us_win_descent              : design_units;
      ul_code_page_range1         : wint option;
      ul_code_page_range2         : wint option;
      s_x_height                  : design_units option;
      s_cap_height                : design_units option;
      us_default_char             : Uchar.t option;
      us_break_char               : Uchar.t option;
      us_lower_optical_point_size : int option;
      us_upper_optical_point_size : int option;
    }
    [@@deriving show]
  end

  (** Defines types for master data in [post] tables. *)
  module Post : sig
    module GlyphNameArray : sig
      type t
      [@@deriving show]

      val access : glyph_id -> t -> string option

      val of_list : string list -> t

      val to_list : t -> string list

      val length : t -> int
    end

    type value = {
      italic_angle        : int;
      underline_position  : int;
      underline_thickness : int;
      is_fixed_pitch      : bool;
      min_mem_type_42     : int;
      max_mem_type_42     : int;
      min_mem_type_1      : int;
      max_mem_type_1      : int;
    }
    [@@deriving show]

    type t = {
      value       : value;
      glyph_names : GlyphNameArray.t option;
    }
    [@@deriving show]
  end

  (** Defines types for master data in [name] tables. *)
  module Name : sig
    type name_record = {
      platform_id : int;
      encoding_id : int;
      language_id : int;
      name_id     : int;
      name        : string;
    }
    [@@deriving show]

    type lang_tag = string
    [@@deriving show]

    type t = {
      name_records : name_record list;
      lang_tags    : (lang_tag list) option;
    }
    [@@deriving show]
  end

  module Ttf : sig
    type contour_element = {
      on_curve : bool;
      point    : point;
    }
    [@@deriving show]

    type contour = contour_element list
    [@@deriving show]

    type composition =
      | Vector   of x_coordinate * y_coordinate
      | Matching of int * int
    [@@deriving show]

    type instruction = string
    [@@deriving show]

    type simple_glyph_description = contour list * instruction
    [@@deriving show]

    type composite_glyph_component = {
      component_glyph_id        : glyph_id;
      composition               : composition;
      component_scale           : linear_transform option;
      round_xy_to_grid          : bool;
      use_my_metrics            : bool;
      unscaled_component_offset : bool;
    }
    [@@deriving show]

    type composite_glyph_description = {
      composite_components  : composite_glyph_component list;
      composite_instruction : instruction option;
    }
    [@@deriving show]

    type glyph_description =
      | SimpleGlyph    of simple_glyph_description
      | CompositeGlyph of composite_glyph_description
    [@@deriving show]

    type glyph_info = {
      bounding_box : bounding_box;
      description  : glyph_description;
    }
    [@@deriving show]
  end

  (** Defines types for master data in [MATH] tables. *)
  module Math : sig
    type math_value_record = int * device option
    [@@deriving show]

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
    [@@deriving show]

    type math_kern = math_value_record list * math_value_record list
    [@@deriving show]

    type math_kern_info_record = {
      top_right_math_kern    : math_kern option;
      top_left_math_kern     : math_kern option;
      bottom_right_math_kern : math_kern option;
      bottom_left_math_kern  : math_kern option;
    }
    [@@deriving show]

    type math_italics_correction = glyph_id * math_value_record
    [@@deriving show]

    type math_glyph_info = {
      math_italics_correction    : math_italics_correction list;
      math_top_accent_attachment : (glyph_id * math_value_record) list;
      math_kern_info             : (glyph_id * math_kern_info_record) list;
    }
    [@@deriving show]

    type glyph_part_record = {
      glyph_id_for_part      : glyph_id;
      start_connector_length : design_units;
      end_connector_length   : design_units;
      full_advance           : design_units;
      f_extender             : bool;
    }
    [@@deriving show]

    type math_glyph_variant_record = {
      variant_glyph       : glyph_id;
      advance_measurement : design_units;
    }
    [@@deriving show]

    type math_glyph_construction = {
      glyph_assembly                 : (math_value_record * glyph_part_record list) option;
      math_glyph_variant_record_list : math_glyph_variant_record list;
    }
    [@@deriving show]

    type math_variants = {
      min_connector_overlap : design_units;
      vert_glyph_assoc      : (glyph_id * math_glyph_construction) list;
      horiz_glyph_assoc     : (glyph_id * math_glyph_construction) list;
    }
    [@@deriving show]

    type t = {
      math_constants  : math_constants;
      math_glyph_info : math_glyph_info;
      math_variants   : math_variants;
    }
    [@@deriving show]
  end
end

(** Handles in-memory representation of OpenType tables
    that contain metadata derived from master data as well as master data. *)
module Intermediate : sig

  (** The type for the [indexToLocFormat] entry in [head] tables. *)
  type loc_format =
    | ShortLocFormat
    | LongLocFormat
  [@@deriving show]

  (** Defines types for representing whole information in [head] tables. *)
  module Head : sig
    (** The type for data contained in a [head] table that are derivable from
        glyph descriptions or master data in other tables. *)
    type derived = {
      x_min               : int;
      y_min               : int;
      x_max               : int;
      y_max               : int;
      index_to_loc_format : loc_format;
    }
    [@@deriving show]

    (** The type for representing [head] tables. *)
    type t = {
      value   : Value.Head.t;
      derived : derived;
    }
    [@@deriving show]
  end

  (** Defines types for representing whole information in [hhea] tables. *)
  module Hhea : sig
    (** The type for data contained in a single [hhea] table that are derivable
        from glyph descriptions or master data in other tables in the font the [hhea] table belongs to. *)
    type derived = {
      advance_width_max      : int;
      min_left_side_bearing  : int;
      min_right_side_bearing : int;
      xmax_extent            : int;
    }
    [@@deriving show]

    (** The type for representing [hhea] tables. *)
    type t = {
      value   : Value.Hhea.t;
      derived : derived;
    }
    [@@deriving show]
  end

  (** Defines types for representing whole information in [OS/2] tables. *)
  module Os2 : sig
    (** The type for data contained in a single [OS/2] table that are derivable
        from glyph descriptions or master data in other tables in the font the [OS/2] table belongs to. *)
    type derived = {
      x_avg_char_width    : Value.design_units;
      us_first_char_index : Uchar.t;
      us_last_char_index  : Uchar.t;
      us_max_context      : int option;
    }
    [@@deriving show]

    (** The type for representing [OS/2] tables. *)
    type t = {
      value   : Value.Os2.t;
      derived : derived;
    }
    [@@deriving show]
  end

  module Ttf : sig
    module Maxp : sig
      (** The type for representing [maxp] tables in fonts that have TrueType-based outlines. *)
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
      [@@deriving show]
    end

    type glyph_location
  end

  module Cff : sig
    module Maxp : sig
      (** The type for representing [maxp] tables in fonts that have CFF-based outlines. *)
      type t = {
        num_glyphs : int;
      }
      [@@deriving show]
    end

    type key =
      | ShortKey of int
      | LongKey  of int
    [@@deriving show]

    (** Represents a bit vector of arbitrary finite length equipped with [hintmask (19)] or [cntrmask (20)]. *)
    type stem_argument = string
    [@@deriving show]

    (** Represents a token in CharStrings. *)
    type charstring_token =
      | ArgumentInteger  of int
      | ArgumentReal     of float

      | OpHStem   (** [hstem (1)] *)
      | OpVStem   (** [vstem (3)] *)
      | OpHStemHM (** [hstemhm (18)] *)
      | OpVStemHM (** [hstemhm (23)] *)

      | OpRMoveTo (** [rmoveto (21)] *)
      | OpHMoveTo (** [hmoveto (22)] *)
      | OpVMoveTo (** [vmoveto (4)] *)

      | OpRLineTo   (** [rlineto (5)] *)
      | OpHLineTo   (** [hlineto (6)] *)
      | OpVLineTo   (** [vlineto (7)] *)

      | OpCallSubr  (** [callsubr (10)] *)
      | OpCallGSubr (** [callgsubr (29)] *)

      | OpReturn  (** [return (11)] *)
      | OpEndChar (** [endchar (14)] *)

      | OpHintMask  of stem_argument (** [hintmask (19)] *)
      | OpCntrMask  of stem_argument (** [cntrmask (20)] *)

      | OpRCurveLine (** [rcurveline (24)] *)
      | OpRLineCurve (** [rlinecurve (25)] *)
      | OpRRCurveTo  (** [rrcurveto (8)] *)
      | OpVVCurveTo  (** [vvcurveto (26)] *)
      | OpHHCurveTo  (** [hhcurveto (27)] *)
      | OpVHCurveTo  (** [vhcurveto (30)] *)
      | OpHVCurveTo  (** [hvcurveto (31)] *)

      | OpHFlex  (** [hflex (12 34)] *)
      | OpFlex   (** [flex (12 35)] *)
      | OpHFlex1 (** [hflex1 (12 36)] *)
      | OpFlex1  (** [flex1 (12 37)] *)
    [@@deriving show]

    type lexical_charstring = charstring_token list
    [@@deriving show]

    (** Represents a relative vector used in CharString. *)
    type vector = Value.design_units * Value.design_units
    [@@deriving show]

    type charstring_operation =
      | HStem of {
          y    : Value.design_units;
          dy   : Value.design_units;
          rest : (Value.design_units * Value.design_units) list;
        } (** [hstem (1)] *)
      | VStem of {
          x    : Value.design_units;
          dx   : Value.design_units;
          rest : (Value.design_units * Value.design_units) list;
        } (** [vstem (3)] *)
      | VMoveTo of {
          dy1 : Value.design_units;
        } (** [vmoveto (4)] *)
      | RLineTo of (Value.design_units * Value.design_units) list
          (** [rlineto (5)] *)
      | HLineTo of Value.design_units list
          (** [hlineto (6)] *)
      | VLineTo of Value.design_units list
          (** [vlineto (7)] *)
      | RRCurveTo of (vector * vector * vector) list
          (** [rrcurveto (8)] *)
      | HStemHM of {
          y    : Value.design_units;
          dy   : Value.design_units;
          rest : (Value.design_units * Value.design_units) list;
        } (** [hstemhm (18)] *)
      | HintMask of stem_argument
          (** [hintmask (19)] *)
      | CntrMask of stem_argument
          (** [cntrmask (20)] *)
      | RMoveTo of {
          dv1 : vector;
        } (** [rmoveto (21)] *)
      | HMoveTo of {
          dx1 : Value.design_units;
        } (** [hmoveto (22)] *)
      | VStemHM of {
          x    : Value.design_units;
          dx   : Value.design_units;
          rest : (Value.design_units * Value.design_units) list;
        } (** [vstemhm (23)] *)
      | VVCurveTo of {
          dx1  : Value.design_units option;
          rest : (Value.design_units * vector * Value.design_units) list;
        } (** [vvcurveto (26)] *)
      | HHCurveTo of {
          dy1  : Value.design_units option;
          rest : (Value.design_units * vector * Value.design_units) list;
        } (** [hhcurveto (27)] *)
      | VHCurveTo of {
          main : (Value.design_units * vector * Value.design_units) list;
          df   : Value.design_units option;
        } (** [vhcurveto (30)] *)
      | HVCurveTo of {
          main : (Value.design_units * vector * Value.design_units) list;
          df   : Value.design_units option;
        } (** [hvcurveto (31)] *)
      | Flex of {
          dv1 : vector;
          dv2 : vector;
          dv3 : vector;
          dv4 : vector;
          dv5 : vector;
          dv6 : vector;
          fd  : Value.design_units;
        } (** [flex (12 35)] *)
      | HFlex of {
          dx1 : Value.design_units;
          dv2 : vector;
          dx3 : Value.design_units;
          dx4 : Value.design_units;
          dx5 : Value.design_units;
          dx6 : Value.design_units;
        } (** [hflex (12 34)] *)
      | HFlex1 of {
          dv1 : vector;
          dv2 : vector;
          dx3 : Value.design_units;
          dx4 : Value.design_units;
          dv5 : vector;
          dx6 : Value.design_units;
        } (** [hflex1 (12 36)] *)
      | Flex1 of {
          dv1 : vector;
          dv2 : vector;
          dv3 : vector;
          dv4 : vector;
          dv5 : vector;
          d6  : Value.design_units;
        } (** [flex1 (12 37)] *)
    [@@deriving show]

    (** Represents a CharString. *)
    type charstring = charstring_operation list
    [@@deriving show]

    (** The type for CIDFont-specific data in Top DICT (CFF p.16, Table 10) *)
    type cid_info = {
      registry          : string;
      ordering          : string;
      supplement        : int;
      cid_font_version  : float;
      cid_font_revision : int;
      cid_font_type     : int;
      cid_count         : int;
    }
    [@@deriving show]

    (** The type for Top DICTs (CFF p.16, Table 10). *)
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
    [@@deriving show]

    type fdindex = int
    [@@deriving show]

    (** Exported for tests. *)
    type charstring_data = CharStringData of int * int
    [@@deriving show]

    (** Exported for tests. *)
    type subroutine_index = charstring_data array
  end
end

(** Contains types and operations for decoding fonts. *)
module Decode : sig
  (** Handles values that represent errors that can happen while decoding fonts. *)
  module Error : sig
    type unsupported_report =
      | CharstringArithmeticOperator of int
    [@@deriving show]

    type charstring_error =
      | UnknownLongOperator        of int
      | StackUnderflow
      | StackRemaining
      | SubroutineIndexOutOfBounds of { index : int; biased : int }
      | NoSubroutineIndex          of string
      | ParsingOverrun             of int
      | NotAMiddleOfPath
      | EmptyRestOfCurve
      | ExceedMaxSubroutineDepth   of int
    [@@deriving show]

    type t =
      | UnknownFormatVersion      of Value.Tag.t
      | UnknownTtcVersion         of wint
      | LayeredTtc
      | InvalidOffset             of offset
      | UnexpectedEnd
      | MissingRequiredTable      of Value.Tag.t
      | UnknownTableVersion       of wint
      | UnsupportedCmapFormat     of int
      | InvalidCodePoint          of int
      | InvalidCodePointRange     of Uchar.t * Uchar.t
      | InvalidLocFormat          of int
      | InvalidCompositeFormat    of int
      | InvalidOffsize            of int
      | InvalidFirstOffsetInIndex of wint
      | NotASingletonIndex
      | NotACffDictElement        of int
      | NotANibbleElement         of int
      | InvalidEndOfReal          of int
      | InconsistentDictLength
      | NotAnIntegerInDict
      | NotARealInDict
      | NotAnIntegerPairInDict
      | NotAQuadrupleInDict
      | UnknownCharstringType     of int
      | RequiredKeyNotFound       of Intermediate.Cff.key
      | InvalidRos
      | SidOutOfBounds            of int
      | NoPrivateDict
      | UnknownFdselectFormat     of int
      | FdindexOutOfBounds        of int
      | FdselectOutOfBounds       of int
      | CharstringWithoutWidth
      | InvalidCharstring         of charstring_error
      | InvalidTtfContour
      | UnknownCoverageFormat     of int
      | InvalidCoverageLength
      | UnknownLookupOrder        of int
      | InvalidFeatureIndex       of int
      | UnknownGsubLookupType     of int
      | UnknownGposLookupType     of int
      | UnknownFormatNumber       of int
      | LayeredExtensionPosition
      | InvalidMarkClass          of int
      | CffContainsTtfMaxpTable
      | TtfContainsCffMaxpTable
      | UnexpectedMacStyle        of int
      | UnknownCharstringToken    of int
      | NegativeLengthForBytes    of int
      | UnknownWeightClass        of int
      | UnknownWidthClass         of int
      | InvalidFsType             of int
      | InvalidFsSelection        of int

      | InconsistentNumberOfPoints of {
          num_points : int;
          num_flags  : int;
          num_xs     : int;
          num_ys     : int;
        }

      | InconsistentNumberOfCmapSegments of {
          seg_count            : int;
          num_end_codes        : int;
          num_start_codes      : int;
          num_id_deltas        : int;
          num_id_range_offsets : int;
        }

      | InvalidCmapSegment of {
          incremental    : bool;
          start_char     : Uchar.t;
          end_char       : Uchar.t;
          start_glyph_id : Value.glyph_id;
        }

      | InvalidReservedPad        of int
      | InvalidGlyphNameIndex     of int
      | Unsupported               of unsupported_report
    [@@deriving show]
  end

  type 'a ok = ('a, Error.t) result

  (** The type for sources equipped with TrueType-based outlines. *)
  type ttf_source

  (** The type for sources equipped with CFF-based outlines. *)
  type cff_source

  (** Represents a single font. *)
  type source =
    | Ttf of ttf_source
    | Cff of cff_source

  (** Represents a single font or a font collection (i.e. so-called a TrueType collection). *)
  type single_or_collection =
    | Single     of source
    | Collection of source list

  (** [source_of_string s] parses [s] as a complete font file. *)
  val source_of_string : string -> single_or_collection ok

  (** Returns the list of tags for the tables contained in the source. *)
  val tables : source -> Value.Tag.t set

  (** Handles intermediate representation of [head] tables for decoding. *)
  module Head : sig
    val get : source -> Intermediate.Head.t ok
  end

  (** Handles intermediate representation of [hhea] tables for decoding. *)
  module Hhea : sig
    val get : source -> Intermediate.Hhea.t ok
  end

  (** Handles intermediate representation of [cmap] tables for decoding.
      Since the operations provided by this module use only sequential sources and
      do NOT allocate so much additional memory for the representation,
      they are likely to be efficient in space. *)
  module Cmap : sig
    (** The type for representing [cmap] tables. *)
    type t

    (** Gets the [cmap] table from a font. *)
    val get : source -> t ok

    (** The type for representing Unicode-aware [cmap] subtables. *)
    type subtable

    (** Gets all the Unicode-aware subtables in the given [cmap] table. *)
    val get_subtables : t -> (subtable set) ok

    val get_subtable_ids : subtable -> Value.Cmap.subtable_ids

    val get_format_number : subtable -> int

    (** Represents an entry in [cmap] subtables, and is used in [fold_subtable].
        [Incremental(uch1, uch2, gid)] maps [uch1] to [gid], [uch1 + 1] to [gid + 1], …, and
        [uch2] to [gid + (uch2 - uch1 + 1)].
        On the other hand, [Constant(uch1, uch2, gid)] maps
        all the code points between [uch1] and [uch2] (including [uch1] and [uch2] themselves) to [gid]. *)
    type segment =
      | Incremental of Uchar.t * Uchar.t * Value.glyph_id
      | Constant    of Uchar.t * Uchar.t * Value.glyph_id

    (** Folds a [cmap] subtable in ascending order. *)
    val fold_subtable : subtable -> ('a -> segment -> 'a) -> 'a -> 'a ok

    (** Converts a [cmap] subtable to an in-memory mapping. *)
    val unmarshal_subtable : subtable -> Value.Cmap.subtable ok
  end

  (** Handles intermediate representation of [hmtx] tables for decoding.
      Since the operations provided by this module use only sequential sources and
      do NOT allocate so much additional memory for the representation,
      they are likely to be efficient in time and space. *)
  module Hmtx : sig
    (** The type for representing [hmtx] tables. *)
    type t

    (** Gets the [hmtx] table of a font. *)
    val get : source -> t ok

    (** [access hmtx gid] returns [Some((aw, lsb))] (if [gid] is present in the font)
        where [aw] is the advance width of [gid], and [lsb] is the left side bearing of [gid]. *)
    val access : t -> Value.glyph_id -> ((Value.design_units * Value.design_units) option) ok
  end

  (** Defines decoding operations for [OS/2] tables. *)
  module Os2 : sig
    (** Gets the [OS/2] table of a font. *)
    val get : source -> Intermediate.Os2.t ok
  end

  (** Defines decoding operations for [post] tables. *)
  module Post : sig
    (** Gets the [post] table of a font. *)
    val get : source -> Value.Post.t ok
  end

  (** Defines decoding operations for [name] tables. *)
  module Name : sig
    (** Gets the [name] table of a font. *)
    val get : source -> Value.Name.t ok
  end

  (** Handles intermediate representation of [GSUB] tables for decoding. *)
  module Gsub : sig
    (** The type for representing [GSUB] tables. *)
    type t

    (** Gets the [GSUB] table of a font if it exists. *)
    val get : source -> (t option) ok

    (** The type for data associated with a Script. *)
    type script

    (** The type for data associated with a LangSys. *)
    type langsys

    (** The type for data associated with a Feature. *)
    type feature

    (** Gets the Script tag (e.g. [DFLT], [cyrl], [hani], [latn], or [kana]). *)
    val get_script_tag : script -> string

    (** Gets the LangSys tag (e.g. [DFLT], [DEU], [FRA], or [TRK]). *)
    val get_langsys_tag : langsys -> string

    (** Gets the [GSUB] Feature tag (e.g. [aalt], [liga], or [onum]). *)
    val get_feature_tag : feature -> string

    (** Returns all the Scripts in a [GSUB] table. *)
    val scripts : t -> (script set) ok

    (** Returns the pair of the default LangSys and the list of all LangSyses supported by a given Script. *)
    val langsyses : script -> (langsys option * langsys set) ok

    (** Returns the pair of the default Feature and the list of all Features supported by a given LangSys. *)
    val features : langsys -> (feature option * feature set) ok

    (** The type for functions that handle entries of
        Single substitution (Lookup Type 1) subtables (OpenType p.251).
        See [fold_subtables]. *)
    type 'a folding_single =
      'a -> Value.glyph_id * Value.glyph_id -> 'a

    (** The type for functions that handle entries of
        Alternate substitution (Lookup Type 3) subtables (OpenType p.253).
        See [fold_subtables]. *)
    type 'a folding_alt =
      'a -> Value.glyph_id * Value.glyph_id list -> 'a

    (** The type for functions that handle entries of
        Ligature substitution (Lookup Type 4) subtables (OpenType p.254).
        See [fold_subtables]. *)
    type 'a folding_lig =
      'a -> Value.glyph_id * (Value.glyph_id list * Value.glyph_id) list -> 'a

    (** Folds all the subtables associated with a [GPOS] Feature.
        Unspecified optional arguments default to a function that remains accumulators unchanged.
        The function [fold_subtables] supports the following types:

        {ul
          {- Lookup Type 1: Single substitution (OpenType p.251),}
          {- Lookup Type 2: Alternate substitution (OpenType p.253), and}
          {- Lookup Type 4: Ligature substitution (OpenType p.254).}}

        Subtables of the following types are ignored:

        {ul
          {- Lookup Type 3: Multiple substitution (OpenType p.252),}
          {- Lookup Type 5: Contextual substitution (OpenType p.255),}
          {- Lookup Type 6: Chaining contextual substitution (OpenType p.261),}
          {- Lookup Type 7: Extension substitution (OpenType p.266), and}
          {- Lookup Type 8: Reverse chaining contextual single substitution (OpenType p.267).}} *)
    val fold_subtables :
      ?single:('a folding_single) ->
      ?alt:('a folding_alt) ->
      ?lig:('a folding_lig) ->
      feature -> 'a -> 'a ok
  end

  (** Handles intermediate representation of [GPOS] tables for decoding. *)
  module Gpos : sig
    (** The type for representing [GPOS] tables. *)
    type t

    (** Gets the [GPOS] table of a font if it exists. *)
    val get : source -> (t option) ok

    (** The type for data associated with a Script. *)
    type script

    (** The type for data associated with a LangSys. *)
    type langsys

    (** The type for data associated with a Feature. *)
    type feature

    (** Gets the Script tag (e.g. [DFLT], [cyrl], [hani], [latn], or [kana]). *)
    val get_script_tag : script -> string

    (** Gets the LangSys tag (e.g. [DFLT], [DEU], [FRA], or [TRK]). *)
    val get_langsys_tag : langsys -> string

    (** Gets the [GPOS] Feature tag (e.g. [kern], [mark], or [mkmk]). *)
    val get_feature_tag : feature -> string

    (** Returns all the Scripts in a [GPOS] table. *)
    val scripts : t -> (script set) ok

    (** Returns the pair of the default LangSys and the list of all LangSyses supported by a given Script. *)
    val langsyses : script -> (langsys option * langsys set) ok

    (** Returns the pair of the default Feature and the list of all Features supported by a given LangSys. *)
    val features : langsys -> (feature option * feature set) ok

    type class_definition =
      | GlyphToClass      of Value.glyph_id * Value.class_value
      | GlyphRangeToClass of Value.glyph_id * Value.glyph_id * Value.class_value
    [@@deriving show]

    (** The type for functions that handle
        Single adjustment positioning (Lookup Type 1) subtables of Format 1 (OpenType p.192).
        See [fold_subtables]. *)
    type 'a folding_single1 =
      'a -> Value.glyph_id list -> Value.value_record -> 'a

    (** The type for functions that handle entries of
        Single adjustment positioning (Lookup Type 1) subtables of Format 2 (OpenType p.193).
        See [fold_subtables]. *)
    type 'a folding_single2 =
      'a -> Value.glyph_id * Value.value_record -> 'a

    (** The type for functions that handle entries of
        Pair adjustment positioning (Lookup Type 2) subtables of Format 1 (OpenType p.194).
        See [fold_subtables]. *)
    type 'a folding_pair1 =
      'a -> Value.glyph_id * (Value.glyph_id * Value.value_record * Value.value_record) list -> 'a

    (** The type for functions that handle
        Pair adjustment positioning (Lookup Type 2) subtables of Format 2 (OpenType p.195).
        See [fold_subtables]. *)
    type 'a folding_pair2 =
      class_definition list -> class_definition list -> 'a ->
        (Value.class_value * (Value.class_value * Value.value_record * Value.value_record) list) list -> 'a

    (** The type for functions that handle
        MarkToBase attachment positioning (Lookup Type 4) subtables of Format 1 (OpenType p.198).
        See [fold_subtables]. *)
    type 'a folding_markbase1 =
      int -> 'a -> (Value.glyph_id * Value.mark_record) list -> (Value.glyph_id * Value.base_record) list -> 'a

    (** The type for functions that handle
        MarkToLigature attachment positioning (Lookup Type 5) subtables of Format 1 (OpenType p.199).
        See [fold_subtables]. *)
    type 'a folding_marklig1 =
      int -> 'a -> (Value.glyph_id * Value.mark_record) list -> (Value.glyph_id * Value.ligature_attach) list -> 'a

    (** The type for functions that handle
        MarkToMark attachment positioning (Lookup Type 6) subtables of Format 1 (OpenType p.201).
        See [fold_subtables]. *)
    type 'a folding_markmark1 =
      int -> 'a -> (Value.glyph_id * Value.mark_record) list -> (Value.glyph_id * Value.mark2_record) list -> 'a

    (** Folds all the subtables associated with a [GPOS] Feature.
        Unspecified optional arguments default to a function that remains accumulators unchanged.
        The function [fold_subtables] supports the following types:

        {ul
          {- Lookup Type 1: Single adjustment positioning (OpenType p.192),}
          {- Lookup Type 2: Pair adjustment positioning (OpenType p.194),}
          {- Lookup Type 4: MarkToBase attachment positioning (OpenType p.198),}
          {- Lookup Type 5: MarkToLigature attachment positioning (OpenType p.199),}
          {- Lookup Type 6: MarkToMark attachment positioning (OpenType p.201), and}
          {- Lookup Type 9: Extension positioning (OpenType p.213).}}

        Subtables of the following types are ignored:

        {ul
          {- Lookup Type 3: Cursive attachment positioning (OpenType p.197),}
          {- Lookup Type 7: Contextual positioning (OpenType p.203), and}
          {- Lookup Type 8: Chaining contextual positioning (OpenType p.209).}} *)
    val fold_subtables :
      ?single1:('a folding_single1) ->
      ?single2:('a folding_single2) ->
      ?pair1:('a folding_pair1) ->
      ?pair2:('a folding_pair2) ->
      ?markbase1:('a folding_markbase1) ->
      ?marklig1:('a folding_marklig1) ->
      ?markmark1:('a folding_markmark1) ->
      feature -> 'a -> 'a ok
  end

  (** Handles intermediate representation of [kern] tables for decoding. *)
  module Kern : sig
    (** The type for representing [kern] tables. *)
    type t

    (** Gets the [kern] table of a font if it exists. *)
    val get : source -> (t option) ok

    type kern_info = {
      horizontal   : bool;
      minimum      : bool;
      cross_stream : bool;
    }
    [@@deriving show]

    val fold : ('a -> kern_info -> bool * 'a) -> ('a -> int -> int -> int -> 'a) -> 'a -> t -> 'a ok
  end

  (** Contains decoding operations for [MATH] tables. *)
  module Math : sig
    (** Gets the [MATH] table of a font if it exists. *)
    val get : source -> (Value.Math.t option) ok
  end

  (** The module for decoding TrueType-based OpenType fonts. *)
  module Ttf : sig
    (** The module for decoding [maxp] tables in TrueType-based fonts. *)
    module Maxp : sig
      val get : ttf_source -> Intermediate.Ttf.Maxp.t ok
    end

    (** [loca ttf gid] consults the [loca] table of the source [ttf] by glyph ID [gid],
        and returns the location of the glyph in the [glyf] table if exists.  *)
    val loca : ttf_source -> Value.glyph_id -> (Intermediate.Ttf.glyph_location option) ok

    (** [glyf ttf loc] accesses [loca] in the [glyf] table and
        returns the bounding box and the outline of the glyph. *)
    val glyf : ttf_source -> Intermediate.Ttf.glyph_location -> Value.Ttf.glyph_info ok

    val path_of_contour : Value.Ttf.contour -> Value.quadratic_path ok
  end

  (** The module for decoding CFF-based OpenType fonts. *)
  module Cff : sig
    (** The module for decoding [maxp] tables in CFF-based fonts. *)
    module Maxp : sig
      val get : cff_source -> Intermediate.Cff.Maxp.t ok
    end

    (** Gets the Top DICT in the [CFF] table. *)
    val top_dict : cff_source -> Intermediate.Cff.top_dict ok

    (** [access_charset cff gid] gets the character name for [gid] stored in the Charset of [cff]. *)
    val access_charset : cff_source -> Value.glyph_id -> (string option) ok

    (** [charstring cff gid] returns:

        {ul
          {- [None] if [gid] is not defined in [cff];}
          {- [Some((width_opt, charstring))] if [gid] is defined in [cff]
             where [width_opt] is the optionally stored advance width of the glyph,
             and [charstring] is the CharString in which subroutine calls have been resolved.}} *)
    val charstring : cff_source -> Value.glyph_id -> ((Value.design_units option * Intermediate.Cff.charstring) option) ok

    (** Returns which FDIndex a given glyph belongs to if the font is a CIDFont, or returns [None] otherwise.
        This function is mainly for experimental use. *)
    val fdindex : cff_source -> Value.glyph_id -> (Intermediate.Cff.fdindex option) ok

    (** Handles subroutine INDEXes that contain low-level CharString representations. *)
    module LexicalSubroutineIndex : sig
      type t
      val empty : t
      val add : int -> Intermediate.Cff.lexical_charstring -> t -> t
      val mem : int -> t -> bool
      val find : int -> t -> Intermediate.Cff.lexical_charstring option
      val fold : (int -> Intermediate.Cff.lexical_charstring -> 'a -> 'a) -> t -> 'a -> 'a
    end

    (** [lexical_charstring cff ~gsubrs ~lsubrs gid] returns [Some((gsubrs_new, lsubrs_new, lcs))] where

        {ul
          {- [cff] is a CFF-based font,}
          {- [gsubrs] and [lsubrs] are maps of already known global subroutines and local ones,}
          {- [gid] is the GID of a glyph (the CharString of which one wants to get),}
          {- [lcs] is the tokenized CharString of [gid] where subroutine calls are not resolved, and}
          {- [gsubrs_new] and [lsubrs_new] are maps made by extending [gsubrs] and [lsubrs]
             while decoding the CharString of [gid].}}

        Here, [gsubrs] and [lsubrs] also enable decoding operations to avoid infinite loops. *)
    val lexical_charstring : cff_source -> gsubrs:LexicalSubroutineIndex.t -> lsubrs:LexicalSubroutineIndex.t -> Value.glyph_id -> ((LexicalSubroutineIndex.t * LexicalSubroutineIndex.t * Intermediate.Cff.lexical_charstring) option) ok

    (** Converts a CharString into a cubic Bézier path. *)
    val path_of_charstring : Intermediate.Cff.charstring -> (Value.cubic_path list) ok
  end
end


module Encode : sig
  (** Handles values that represent errors that can happen while encoding fonts. *)
  module Error : sig
    type unsupported_report =
      | LocalSubrOperation
    [@@deriving show]

    type t =
      | NotEncodableAsUint8        of int
      | NotEncodableAsInt8         of int
      | NotEncodableAsUint16       of int
      | NotEncodableAsInt16        of int
      | NotEncodableAsUint24       of int
      | NotEncodableAsUint32       of wint
      | NotEncodableAsInt32        of wint
      | NotEncodableAsTimestamp    of wint
      | NotA10BytePanose           of string
      | NotA4ByteAchVendId         of string
      | Version4FsSelection        of Value.Os2.fs_selection
      | TooLargeToDetermineOffSize of int
      | NotEncodableAsDictValue    of int
      | InvalidNumberOfGlyphNames  of { expected : int; got : int }
      | Unsupported                of unsupported_report
    [@@deriving show]
  end

  type 'a ok = ('a, Error.t) result

  (** The type for encoded tables. Values of this type is basically
      a pair of a 4cc tag and a raw string of table contents. *)
  type table

  val get_table_tag : table -> Value.Tag.t

  val get_contents : table -> string

  (** Make an entire font file from tables. This function performs:
      - sorting tables based on 4cc tags,
      - the insertion of padding [0x00] in order for every table to begin at the offset of multiples of 4,
      - the calculation of checksums of each table, and
      - the insertion of the sfnt header and the table directory,
      - the calculation of checkSumAdjustment.

      On the other hand, it does NOT perform, for example:
      - checking whether all the required tables are present, or
      - checking that some derived data encoded in given tables are consistent with master data. *)
  val make_font_data_from_tables : ttf:bool -> table list -> string ok

  (** Defines encoding operations for [head] tables. *)
  module Head : sig
    val make : Intermediate.Head.t -> table ok
  end

  (** Defines encoding operations for [hhea] tables. *)
  module Hhea : sig
    val make : number_of_h_metrics:int -> Intermediate.Hhea.t -> table ok
  end

  (** Defines encoding operations for [OS/2] tables. *)
  module Os2 : sig
    val make : Intermediate.Os2.t -> table ok
  end

  (** Defines encoding operations for [post] tables. *)
  module Post : sig
    val make : num_glyphs:int -> Value.Post.t -> table ok
  end

  (** Defines encoding operations for [name] tables. *)
  module Name : sig
    val make : Value.Name.t -> table ok
  end

  (** Defines encoding operations for [hmtx] tables. *)
  module Hmtx : sig
    val make_exact : (Value.design_units * Value.design_units) list -> Value.design_units list -> table ok
    val make : (Value.design_units * Value.design_units) list -> table ok
  end

  (** Defines encoding operations for [cmap] tables. *)
  module Cmap : sig
    val make : Value.Cmap.t -> table ok
  end

  (** Defines encoding operations for tables specific to TrueType-based fonts. *)
  module Ttf : sig
    (** Defines encoding operations for [maxp] tables of TrueType-based fonts. *)
    module Maxp : sig
      val make : Intermediate.Ttf.Maxp.t -> table ok
    end

    type glyph_relative_offset

    (** Encodes a [glyf] table and produces glyph locations that will constitute a [loca] table. *)
    val make_glyf : Value.Ttf.glyph_info list -> (table * glyph_relative_offset list) ok

    (** Encodes a [loca] table and produces a value for the [indexToLocFormat] entry of the [head] table. *)
    val make_loca : glyph_relative_offset list -> (table * Intermediate.loc_format) ok
  end

  (** Defines encoding operations for tables specific to CFF-based fonts. *)
  module Cff : sig
    (** Defines encoding operations for [maxp] tables of CFF-based fonts. *)
    module Maxp : sig
      val make : Intermediate.Cff.Maxp.t -> table ok
    end

    (** Encodes a [CFF] table. *)
    val make :
      Intermediate.Cff.top_dict ->
      gsubrs:(Intermediate.Cff.lexical_charstring list) ->
      names_and_charstrings:((string * Intermediate.Cff.lexical_charstring) list) ->
      table ok
  end
end

(** Contains operations for encoding subset fonts. *)
module Subset : sig
  (** Handles values that represent errors that can happen while encoding subset fonts. *)
  module Error : sig
    type t =
      | NoGlyphGiven
      | GlyphNotFound               of Value.glyph_id
      | DependentGlyphNotFound      of { depending : Value.glyph_id; depended : Value.glyph_id }
      | DecodeError                 of Decode.Error.t
      | EncodeError                 of Encode.Error.t
      | NonexplicitSubroutineNumber
    [@@deriving show]
  end

  type 'a ok = ('a, Error.t) result

  (** [make d gids] encodes a subset font of [d] that contains glyphs corresponding to [gids] only.
      The resulting subset font consists only of mandatory tables. *)
  val make : ?omit_cmap:bool -> Decode.source -> Value.glyph_id list -> string ok
end
