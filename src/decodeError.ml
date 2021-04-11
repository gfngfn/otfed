
open Basic

type unsupported_report =
  | CharstringArithmeticOperator of int
[@@deriving show { with_path = false }]

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
      [@printer (fun ppf (uch1, uch2) -> Format.fprintf ppf "InvalidCodePointRange(%04x, %04x)" (Uchar.to_int uch1) (Uchar.to_int uch2))]
  | InvalidLocFormat          of int
  | InvalidCompositeFormat    of int
  | InvalidOffsize            of int
  | InvalidFirstOffsetInIndex of wint
  | NotASingletonIndex
  | NotACffDictElement
  | InconsistentDictLength
  | NotAnIntegerInDict
  | NotARealInDict
  | NotAnIntegerPairInDict
  | NotAQuadrupleInDict
  | UnknownCharstringType     of int
  | RequiredKeyNotFound
  | InvalidRos
  | SidOutOfBounds            of int
  | NoPrivateDict
  | UnknownFdselectFormat     of int
  | FdindexOutOfBounds        of int
  | FdselectOutOfBounds       of int
  | CharstringWithoutWidth
  | InvalidCharstring
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
      start_char     : Uchar.t; [@printer pp_uchar]
      end_char       : Uchar.t; [@printer pp_uchar]
      start_glyph_id : Value.glyph_id;
    }

  | InvalidReservedPad        of int
  | InvalidGlyphNameIndex     of int
  | Unsupported               of unsupported_report
[@@deriving show { with_path = false }]
