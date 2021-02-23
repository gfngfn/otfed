
open Basic


type t =
  | UnknownFormatVersion of Value.Tag.t
  | InvalidOffset        of offset
  | UnexpectedEnd
