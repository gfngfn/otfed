
open Basic

include EncodeTable

include EncodeBasic


module ForTest = struct
  module EncodeOperation = EncodeOperation

  type 'a encoder = 'a EncodeOperation.Open.encoder

  let run enc =
    let open ResultMonad in
    enc |> EncodeOperation.run >>= fun (contents, _) ->
    return contents
end
