val message : string

type 'a set = 'a list

module Value : sig
  module Tag : sig
    type t
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

  module Intermediate : sig
    module Cmap : sig
      type t

      type subtable

      val get_subtables : t -> (subtable set) ok
    end
  end

  val source_of_string : string -> source ok

  val cmap : common_source -> Intermediate.Cmap.t ok
end
