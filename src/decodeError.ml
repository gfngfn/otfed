
open Basic


type t =
  | UnknownFormatVersion  of Value.Tag.t
  | UnknownTtcVersion     of wint
  | LayeredTtc
  | InvalidOffset         of offset
  | UnexpectedEnd
  | MissingRequiredTable  of Value.Tag.t
  | UnknownTableVersion   of wint
  | UnsupportedCmapFormat of int
  | InvalidCodePoint      of int
  | InvalidCodePointRange of Uchar.t * Uchar.t
      [@printer (fun ppf (uch1, uch2) -> Format.fprintf ppf "InvalidCodePointRange(%04x, %04x)" (Uchar.to_int uch1) (Uchar.to_int uch2))]
  | InvalidLocFormat      of int
  | InvalidCompositeFormat of int
  | InvalidOffsize         of int
[@@deriving show { with_path = false }]
