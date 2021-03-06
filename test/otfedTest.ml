
module T = Otfed.Decode.ForTest


let assert_match msg expected = function
  | Ok(got)  -> if got = expected then () else failwith msg
  | Error(_) -> failwith msg


let () =
  let res = T.run TestCaseGlyf1.data T.d_glyf in
  assert_match "glyf" TestCaseGlyf1.expected res
