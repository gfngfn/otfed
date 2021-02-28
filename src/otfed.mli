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

  type loc_format =
    | ShortLocFormat
    | LongLocFormat

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
      index_to_loc_format : loc_format;
    }
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
  end
end

module Decode : sig
  module Error : sig
    type t
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

  (** [source_of_string s] parses [s] as a complete font file. *)
  val source_of_string : string -> single_or_collection ok

  (** Returns the list of tags for the tables contained in the source. *)
  val tables : common_source -> Value.Tag.t set

  (** Used in [Intermediate.Cmap.fold_subtable]. *)
  type cmap_segment =
    | Incremental of Uchar.t * Uchar.t * Value.glyph_id
    | Constant    of Uchar.t * Uchar.t * Value.glyph_id

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
end
