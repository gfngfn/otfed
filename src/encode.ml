
open Basic

include EncodeTable

include EncodeBasic


module Subset = EncodeSubset


module ForTest = struct
  module EncodeOperation = EncodeOperation

  type 'a encoder = 'a EncodeOperation.Open.encoder

  let run enc =
    let open ResultMonad in
    enc |> EncodeOperation.run >>= fun (contents, _) ->
    return contents
end
