
module Error = DecodeError

module Intermediate = DecodeIntermediate

type 'a ok = ('a, Error.t) result

type common_source = unit (* TODO *)

type ttf_source = unit (* TODO *)

type cff_source = unit (* TODO *)

type specific_source =
  | Ttf of ttf_source
  | Cff of cff_source

type source = common_source * specific_source
