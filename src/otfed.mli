val message : string

type 'a set = 'a list

module ResultMonad : sig
  val ( >>= ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  val return : 'a -> ('a, 'e) result
  val err : 'e -> ('a, 'e) result
  val mapM : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
end

module Value : sig
  module Tag : sig
    type t
    val to_string : t -> string
  end

  type glyph_id = int

  module Cmap : sig
    type t

    type subtable

    type subtable_ids = {
      platform_id : int;
      encoding_id : int;
      format      : int;
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

  val source_of_string : string -> single_or_collection ok

  val tables : common_source -> Value.Tag.t set

  module Intermediate : sig
    module Cmap : sig
      type t

      type subtable

      val get_subtables : t -> (subtable set) ok
    end
  end

  val cmap : common_source -> Intermediate.Cmap.t ok
end
