
include module type of Basic

(** Handles master data for OpenType fonts. *)
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

  type timestamp = wint

  type ttf_contour = (bool * x_coordinate * y_coordinate) list
  [@@deriving show]

  type linear_transform = {
    a : float;
    b : float;
    c : float;
    d : float;
  }
  [@@deriving show]

  type composition =
    | Vector   of x_coordinate * y_coordinate
    | Matching of int * int
  [@@deriving show]

  type ttf_simple_glyph_description = ttf_contour list
  [@@deriving show]

  type ttf_composite_glyph_description = (glyph_id * composition * linear_transform option) list
  [@@deriving show]

  type ttf_glyph_description =
    | TtfSimpleGlyph    of ttf_simple_glyph_description
    | TtfCompositeGlyph of ttf_composite_glyph_description
  [@@deriving show]

  type bounding_box = {
    x_min : x_coordinate;
    y_min : y_coordinate;
    x_max : x_coordinate;
    y_max : y_coordinate;
  }
  [@@deriving show]

  type ttf_glyph_info = {
    bounding_box : bounding_box;
    description  : ttf_glyph_description;
  }

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

  (** [unite_bounding_boxes bbox1 bbox2] returns the minimum bounding box that includes both [bbox1] and [bbox2]. *)
  val unite_bounding_boxes : bounding_box -> bounding_box -> bounding_box

  (** [bounding_box_by_points pt1 pt2] returns the minimum bounding box that contains both [pt1] and [pt2]. *)
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

  module Cmap : (module type of Value.Cmap)

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

  module Os2 : sig
    type t = {
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
      us_lower_optical_point_size : int option;
      us_upper_optical_point_size : int option;
    }
    [@@deriving show]
  end

  module Post : (module type of Value.Post)

  module Name : (module type of Value.Name)

  module Math : (module type of Value.Math)
end

(** Handles in-memory representation of OpenType tables that contain metadata derived from master data. *)
module Intermediate : sig
  type loc_format =
    | ShortLocFormat
    | LongLocFormat
  [@@deriving show]

  module Head : sig
    (** The type for data contained in a single [head] table that are derivable
        from glyph descriptions or master data in other tables in the font the [head] table belongs to. *)
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

  module Os2 : sig
    (** The type for data contained in a single [OS/2] table that are derivable
        from glyph descriptions or master data in other tables in the font the [OS/2] table belongs to. *)
    type derived = {
      x_avg_char_width    : int;
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

    type offsize = OffSize1 | OffSize2 | OffSize3 | OffSize4

    type key =
      | ShortKey of int
      | LongKey  of int
    [@@deriving show]

    type value =
      | Integer of int
      | Real    of float

    type dict_element =
      | Value of value
      | Key   of key

    module DictMap : Map.S

    (** The type for DICT data (CFF p.9, Section 4) *)
    type dict = (value list) DictMap.t

    (** Represents a bit vector of arbitrary finite length equipped with [hintmask (19)] or [cntrmask (20)]. *)
    type stem_argument = string

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

    (** Represents a relative vector. *)
    type vector = Value.design_units * Value.design_units
    [@@deriving show { with_path = false }]

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

    (** The type for Top DICT (CFF p.16, Table 10). *)
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
      font_bbox           : int * int * int * int;
      stroke_width        : int;
      cid_info            : cid_info option;
      number_of_glyphs    : int;
    }
    [@@deriving show]
  end
end

module Decode : sig
  module Error : (module type of DecodeError)

  type 'a ok = ('a, Error.t) result

  (** The type for sources equipped with TrueType-based outlines. *)
  type ttf_source

  (** The type for sources equipped with CFF-based outlines. *)
  type cff_source

  type source =
    | Ttf of ttf_source
    | Cff of cff_source

  type single_or_collection =
    | Single     of source
    | Collection of source list

  (** [source_of_string s] parses [s] as a complete font file. *)
  val source_of_string : string -> single_or_collection ok

  (** Returns the list of tags for the tables contained in the source. *)
  val tables : source -> Value.Tag.t set

  (** Used in [Intermediate.Cmap.fold_subtable]. *)
  type cmap_segment =
    | Incremental of Uchar.t * Uchar.t * Value.glyph_id
    | Constant    of Uchar.t * Uchar.t * Value.glyph_id

  type fdindex = int

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

    val get : source -> t ok

    type subtable

    val get_subtables : t -> (subtable set) ok

    val get_subtable_ids : subtable -> Value.Cmap.subtable_ids

    val get_format_number : subtable -> int

    val fold_subtable : subtable -> ('a -> cmap_segment -> 'a) -> 'a -> 'a ok

    val unmarshal_subtable : subtable -> Value.Cmap.subtable ok
  end

  (** Handles intermediate representation of [hmtx] tables for decoding.
      Since the operations provided by this module use only sequential sources and
      do NOT allocate so much additional memory for the representation,
      they are likely to be efficient in space. *)
  module Hmtx : sig
    (** The type for representing [hmtx] tables. *)
    type t

    val get : source -> t ok

    val access : t -> Value.glyph_id -> ((int * int) option) ok
  end

  (** Handles intermediate representation of [OS/2] tables for decoding. *)
  module Os2 : sig
    val get : source -> Intermediate.Os2.t ok
  end

  module Post : sig
    val get : source -> Value.Post.t ok
  end

  module Name : sig
    val get : source -> Value.Name.t ok
  end

  (** Handles intermediate representation of [GSUB] tables for decoding. *)
  module Gsub : sig
    (** The type for representing [GSUB] tables. *)
    type t

    val get : source -> (t option) ok

    type script

    type langsys

    type feature

    val get_script_tag : script -> string

    val get_langsys_tag : langsys -> string

    val get_feature_tag : feature -> string

    val scripts : t -> (script set) ok

    val langsyses : script -> (langsys option * langsys set) ok

    val features : langsys -> (feature option * feature set) ok

    type 'a folding_single =
      'a -> Value.glyph_id * Value.glyph_id -> 'a

    type 'a folding_alt =
      'a -> Value.glyph_id * Value.glyph_id list -> 'a

    type 'a folding_lig =
      'a -> Value.glyph_id * (Value.glyph_id list * Value.glyph_id) list -> 'a

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

    val get : source -> (t option) ok

    type script

    type langsys

    type feature

    val get_script_tag : script -> string

    val get_langsys_tag : langsys -> string

    val get_feature_tag : feature -> string

    val scripts : t -> (script set) ok

    val langsyses : script -> (langsys option * langsys set) ok

    val features : langsys -> (feature option * feature set) ok

    type class_definition =
      | GlyphToClass      of Value.glyph_id * Value.class_value
      | GlyphRangeToClass of Value.glyph_id * Value.glyph_id * Value.class_value
    [@@deriving show]

    type 'a folding_single1 =
      'a -> Value.glyph_id list -> Value.value_record -> 'a

    type 'a folding_single2 =
      'a -> Value.glyph_id * Value.value_record -> 'a

    type 'a folding_pair1 =
      'a -> Value.glyph_id * (Value.glyph_id * Value.value_record * Value.value_record) list -> 'a

    type 'a folding_pair2 =
      class_definition list -> class_definition list -> 'a ->
        (Value.class_value * (Value.class_value * Value.value_record * Value.value_record) list) list -> 'a

    type 'a folding_markbase1 =
      int -> 'a -> (Value.glyph_id * Value.mark_record) list -> (Value.glyph_id * Value.base_record) list -> 'a

    type 'a folding_marklig1 =
      int -> 'a -> (Value.glyph_id * Value.mark_record) list -> (Value.glyph_id * Value.ligature_attach) list -> 'a

    type 'a folding_markmark1 =
      int -> 'a -> (Value.glyph_id * Value.mark_record) list -> (Value.glyph_id * Value.mark2_record) list -> 'a

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
    val get : source -> (Value.Math.t option) ok
  end

  module Ttf : sig
    module Maxp : sig
      val get : ttf_source -> Intermediate.Ttf.Maxp.t ok
    end

    (** [loca ttf gid] consults the [loca] table of the source [ttf] by glyph ID [gid],
        and returns the location of the glyph in the [glyf] table if exists.  *)
    val loca : ttf_source -> Value.glyph_id -> (Intermediate.Ttf.glyph_location option) ok

    (** [glyf ttf loc] accesses [loca] in the [glyf] table and
        returns the bounding box and the outline of the glyph. *)
    val glyf : ttf_source -> Intermediate.Ttf.glyph_location -> Value.ttf_glyph_info ok

    val path_of_ttf_contour : Value.ttf_contour -> Value.quadratic_path ok
  end

  module Cff : sig
    module Maxp : sig
      val get : cff_source -> Intermediate.Cff.Maxp.t ok
    end

    val top_dict : cff_source -> Intermediate.Cff.top_dict ok

    val access_charset : cff_source -> Value.glyph_id -> (string option) ok

    val charstring : cff_source -> Value.glyph_id -> ((int option * Intermediate.Cff.charstring) option) ok

    val fdindex : cff_source -> Value.glyph_id -> (fdindex option) ok

    module LexicalSubroutineIndex : sig
      type t
      val empty : t
      val add : int -> Intermediate.Cff.lexical_charstring -> t -> t
      val mem : int -> t -> bool
      val find : int -> t -> Intermediate.Cff.lexical_charstring option
      val fold : (int -> Intermediate.Cff.lexical_charstring -> 'a -> 'a) -> t -> 'a -> 'a
    end

    val lexical_charstring : cff_source -> gsubrs:LexicalSubroutineIndex.t -> lsubrs:LexicalSubroutineIndex.t -> Value.glyph_id -> ((LexicalSubroutineIndex.t * LexicalSubroutineIndex.t * Intermediate.Cff.lexical_charstring) option) ok

    val path_of_charstring : Intermediate.Cff.charstring -> (Value.cubic_path list) ok
  end

  type charstring_data = CharStringData of int * int
  type subroutine_index = charstring_data array
  module ForTest : sig
    module DecodeOperation : (module type of DecodeOperation)
    type 'a decoder = 'a DecodeOperation.Open.decoder
    val run : string -> 'a decoder -> 'a ok
    val d_glyf : Value.ttf_glyph_info decoder
    val chop_two_bytes : data:int -> unit_size:int -> repeat:int -> int list
    val run_d_charstring :
      gsubr_index:subroutine_index ->
      lsubr_index:subroutine_index ->
      string -> start:int -> charstring_length:int -> (Intermediate.Cff.charstring_operation list) ok
  end
end


module Encode : sig
  module Error : (module type of EncodeError)

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

  module Head : sig
    val make : Intermediate.Head.t -> table ok
  end

  module Hhea : sig
    val make : number_of_h_metrics:int -> Intermediate.Hhea.t -> table ok
  end

  module Os2 : sig
    val make : Intermediate.Os2.t -> table ok
  end

  module Post : sig
    val make : Value.Post.t -> table ok
  end

  module Name : sig
    val make : Value.Name.t -> table ok
  end

  module Hmtx : sig
    val make : (Value.design_units * Value.design_units) list -> table ok
  end

  module Cmap : sig
    val make : Value.Cmap.t -> table ok
  end

  module Ttf : sig
    module Maxp : sig
      val make : Intermediate.Ttf.Maxp.t -> table ok
    end

    val make_glyf : Value.ttf_glyph_info list -> (table * Intermediate.Ttf.glyph_location list) ok

    val make_loca : Intermediate.Ttf.glyph_location list -> (table * Intermediate.loc_format) ok
  end

  module Cff : sig
    module Maxp : sig
      val make : Intermediate.Cff.Maxp.t -> table ok
    end

    val make :
      Intermediate.Cff.top_dict ->
      gsubrs:(Intermediate.Cff.lexical_charstring list) ->
      names_and_charstrings:((string * Intermediate.Cff.lexical_charstring) list) ->
      table ok
  end

  module ForTest : sig
    module EncodeOperation : (module type of EncodeOperation)
    type 'a encoder = 'a EncodeOperation.Open.encoder
    val run : 'a encoder -> string ok
  end
end

module Subset : sig
  type error =
    | NoGlyphGiven
    | GlyphNotFound of Value.glyph_id
    | DependentGlyphNotFound of { depending : Value.glyph_id; depended : Value.glyph_id }
    | DecodeError   of Decode.Error.t
    | EncodeError   of Encode.Error.t
    | NonexplicitSubroutineNumber
  [@@deriving show]

  type 'a ok = ('a, error) result

  val make : ?omit_cmap:bool -> Decode.source -> Value.glyph_id list -> string ok
end
