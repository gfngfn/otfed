
module Error = EncodeError
module Subset = EncodeSubset


type 'a ok = ('a, Error.t) result
