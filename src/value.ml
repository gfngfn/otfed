
open Basic


module Tag : sig
  type t
  val equal : t -> t -> bool
  val of_wide_int : wint -> t
  val format_version_OTTO : t
  val format_version_true : t
  val format_version_1_0 : t
  val format_version_ttcf : t
end = struct
  type t = wint

  let equal = WideInt.equal

  let of_wide_int n = n

  let format_version_OTTO = !%% 0x4F54544FL
  let format_version_true = !%% 0x74727565L
  let format_version_1_0  = !%% 0x00010000L
  let format_version_ttcf = !%% 0x74746366L
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
