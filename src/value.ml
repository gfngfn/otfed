
module Tag = struct
  type t = unit (* TODO *)
end

type glyph_id = int

module Cmap = struct
  type t

  type subtable

  type subtable_ids = {
    platform_id : int;
    encoding_id : int;
    format      : int;
  }
end
