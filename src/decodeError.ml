
open Basic


type t =
  | UnknownFormatVersion of Value.Tag.t
  | UnknownTtcVersion    of wint
  | LayeredTtc
  | InvalidOffset        of offset
  | UnexpectedEnd
  | MissingRequiredTable of Value.Tag.t
  | UnknownTableVersion  of wint
