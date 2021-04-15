
open Basic

type t =
  | NotEncodableAsUint8     of int
  | NotEncodableAsInt8      of int
  | NotEncodableAsUint16    of int
  | NotEncodableAsInt16     of int
  | NotEncodableAsUint24    of int
  | NotEncodableAsUint32    of wint
  | NotEncodableAsInt32     of wint
  | NotEncodableAsTimestamp of wint
  | NotA10BytePanose        of string
  | NotA4ByteAchVendId      of string
  | TooLargeToDetermineOffSize of int
  | NotEncodableAsDictValue    of int
[@@deriving show { with_path = false }]
