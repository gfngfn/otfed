
open Basic

type t =
  | NotEncodableAsUint8  of int
  | NotEncodableAsInt8   of int
  | NotEncodableAsUint16 of int
  | NotEncodableAsInt16  of int
  | NotEncodableAsUint24 of int
  | NotEncodableAsUint32 of wint
  | NotEncodableAsInt32  of wint
