
open Basic


module Tag : sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_wide_int : wint -> t
  val to_string : t -> string
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
end

type glyph_id = int

type timestamp = wint

type loc_format =
  | ShortLocFormat
  | LongLocFormat

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
    xmin                : int;
    ymin                : int;
    xmax                : int;
    ymax                : int;
    mac_style           : int;
    lowest_rec_ppem     : int;
    index_to_loc_format : loc_format;
  }

end
