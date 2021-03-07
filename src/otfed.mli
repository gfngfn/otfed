val message : string

include module type of Basic

(** Handles unmarshaled in-memory representation of tables. *)
module Value : sig

  (** Handles 4cc tags. *)
  module Tag : sig
    type t
    val to_string : t -> string
  end

  type glyph_id = int

  type timestamp = wint

  type ttf_contour = (bool * int * int) list
  [@@deriving show { with_path = false }]

  type linear_transform = {
    a : float;
    b : float;
    c : float;
    d : float;
  }
  [@@deriving show { with_path = false }]

  type composition =
    | Vector   of int * int
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
    x_min : int;
    y_min : int;
    x_max : int;
    y_max : int;
  }
  [@@deriving show { with_path = false }]

  module Cmap : sig
    type t

    type subtable

    type subtable_ids = {
      platform_id : int;
      encoding_id : int;
      format      : int;
    }
  end

  module Head : sig
    type t = {
      font_revision       : wint;
      flags               : int;
      units_per_em        : int;
      created             : timestamp;
      modified            : timestamp;
      xmin                : int;
      ymin                : int;
      xmax                : int;
      ymax                : int;
      mac_style           : int;
      lowest_rec_ppem     : int;
    }
    [@@deriving show {with_path = false}]
  end

  module Hhea : sig
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

  module Os2 : sig
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
    [@@deriving show { with_path = false }]
  end

  module Maxp : sig
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
end

module Decode : sig
  module Error : sig
    include module type of DecodeError
  end

  type 'a ok = ('a, Error.t) result

  type common_source

  type ttf_source

  type cff_source

  type specific_source =
    | Ttf of ttf_source
    | Cff of cff_source

  type source = common_source * specific_source

  type single_or_collection =
    | Single     of source
    | Collection of source list

  type ttf_glyph_location

  (** [source_of_string s] parses [s] as a complete font file. *)
  val source_of_string : string -> single_or_collection ok

  (** Returns the list of tags for the tables contained in the source. *)
  val tables : common_source -> Value.Tag.t set

  (** Used in [Intermediate.Cmap.fold_subtable]. *)
  type cmap_segment =
    | Incremental of Uchar.t * Uchar.t * Value.glyph_id
    | Constant    of Uchar.t * Uchar.t * Value.glyph_id

  (** Represents a bit vector of arbitrary finite length equipped with [hintmask (19)] or [cntrmask (20)]. *)
  type stem_argument = string

  type csx = int

  type csy = int

  type cspoint = csx * csy

  type parsed_charstring =
    | HStem     of int * int * cspoint list                                        (** [hstem (1)] *)
    | VStem     of int * int * cspoint list                                        (** [vstem (3)] *)
    | VMoveTo   of int                                                             (** [vmoveto (4)] *)
    | RLineTo   of cspoint list                                                    (** [rlineto (5)] *)
    | HLineTo   of int list                                                        (** [hlineto (6)] *)
    | VLineTo   of int list                                                        (** [vlineto (7)] *)
    | RRCurveTo of (cspoint * cspoint * cspoint) list                              (** [rrcurveto (8)] *)
    | HStemHM   of int * int * cspoint list                                        (** [hstemhm (18)] *)
    | HintMask  of stem_argument                                                   (** [hintmask (19)] *)
    | CntrMask  of stem_argument                                                   (** [cntrmask (20)] *)
    | RMoveTo   of cspoint                                                         (** [rmoveto (21)] *)
    | HMoveTo   of int                                                             (** [hmoveto (22)] *)
    | VStemHM   of int * int * cspoint list                                        (** [vstemhm (23)] *)
    | VVCurveTo of csx option * (csy * cspoint * csy) list                         (** [vvcurveto (26)] *)
    | HHCurveTo of csy option * (csx * cspoint * csx) list                         (** [hhcurveto (27)] *)
    | VHCurveTo of (int * cspoint * int) list * int option                         (** [vhcurveto (30)] *)
    | HVCurveTo of (int * cspoint * int) list * int option                         (** [hvcurveto (31)] *)
    | Flex      of cspoint * cspoint * cspoint * cspoint * cspoint * cspoint * int (** [flex (12 35)] *)
    | HFlex     of int * cspoint * int * int * int * int                           (** [hflex (12 34)] *)
    | HFlex1    of cspoint * cspoint * int * int * cspoint * int                   (** [hflex1 (12 36)] *)
    | Flex1     of cspoint * cspoint * cspoint * cspoint * cspoint * int           (** [flex1 (12 37)] *)

  (** Handles intermediate representation of tables for decoding.
      Since the operations provided by this module
      use only sequential sources and
      do NOT allocate so much additional memory for the representation,
      it is likely to be efficient in space and NOT to be in time. *)
  module Intermediate : sig
    module Cmap : sig
      type t

      type subtable

      val get_subtables : t -> (subtable set) ok

      val get_subtable_ids : subtable -> Value.Cmap.subtable_ids

      val fold_subtable : subtable -> ('a -> cmap_segment -> 'a) -> 'a -> 'a ok
    end
  end

  val cmap : common_source -> Intermediate.Cmap.t ok

  val head : common_source -> Value.Head.t ok

  val hhea : common_source -> Value.Hhea.t ok

  val os2 : common_source -> Value.Os2.t ok

  val maxp : common_source -> Value.Maxp.t ok

  val loca : ttf_source -> Value.glyph_id -> (ttf_glyph_location option) ok

  val glyf : ttf_source -> ttf_glyph_location -> (Value.ttf_glyph_description * Value.bounding_box) ok

  val charstring : cff_source -> Value.glyph_id -> ((int option * parsed_charstring list) option) ok

  module ForTest : sig
    type 'a decoder
    val run : string -> 'a decoder -> 'a ok
    val d_glyf : (Value.ttf_glyph_description * Value.bounding_box) decoder
  end
end
