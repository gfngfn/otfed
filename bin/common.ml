
module Alist = Otfed.Alist
module ResultMonad = Otfed.ResultMonad
module D = Otfed.Decode
module E = Otfed.Encode
module V = Otfed.Value
module I = Otfed.Intermediate
module Subset = Otfed.Subset


type error =
  | UnknownCommand     of string
  | InvalidCommandLine
  | InvalidUtf8String
  | CannotReadFile     of string
  | CannotWriteFile    of string
  | DecodingError      of D.Error.t
  | SubsetError        of Subset.error
[@@deriving show { with_path = false }]


let inj x =
  x |> Result.map_error (fun e -> DecodingError(e))

let inj_subset x =
  x |> Result.map_error (fun e -> SubsetError(e))
