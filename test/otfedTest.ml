
module V = Otfed.Value
module D = Otfed.Decode
module T = Otfed.Decode.ForTest


let assert_match ~pp ~message:msg expected = function
  | Ok(got) ->
      if got = expected then
        print_endline "OK"
      else begin
        Format.printf "@[<v>";
        Format.printf "%s@," msg;
        Format.printf "expected:@,@[<h>%a,@]@," pp expected;
        Format.printf "got:@,@[<h>%a@]@," pp got;
        Format.printf "@]";
        exit 1
      end

  | Error(e) ->
      Format.printf "%s\n" msg;
      Format.printf "%a\n" Otfed.Decode.Error.pp e;
      exit 1


let () =
  let pp ppf (descr, bbox) =
    Format.fprintf ppf "(%a, %a)" V.pp_ttf_glyph_description descr V.pp_bounding_box bbox
  in
  let res = T.run TestCaseGlyf1.data T.d_glyf in
  assert_match ~pp ~message:"glyf" TestCaseGlyf1.expected res


let () =
  let (gsize, gkeyval) = TestCaseCff1.gsubrs in
  let gsubr_index = Array.make gsize (D.CharStringData(0, 0)) in
  let (lsize, lkeyval) = TestCaseCff1.lsubrs in
  let lsubr_index = Array.make lsize (D.CharStringData(0, 0)) in
  let (start, data, charstring_length) =
    let buf = Buffer.create 1024 in
    let start =
      gkeyval |> List.fold_left (fun start (i, s) ->
        let len = String.length s in
        Format.printf "Write: global, biased = %d, offset = %d, length = %d\n" i start len;
        gsubr_index.(i) <- D.CharStringData(start, len);
        Buffer.add_string buf s;
        start + len
      ) 0
    in
    let start =
      lkeyval |> List.fold_left (fun start (i, s) ->
        let len = String.length s in
        Format.printf "Write: local, biased = %d, offset = %d, length = %d\n" i start len;
        lsubr_index.(i) <- D.CharStringData(start, len);
        Buffer.add_string buf s;
        start + len
      ) start
    in
    let charstring_length = String.length TestCaseCff1.charstring_data in
    Format.printf "start: %d, charstring_length: %d\n" start charstring_length;
    Buffer.add_string buf TestCaseCff1.charstring_data;
    let data = Buffer.contents buf in
    (start, data, charstring_length)
  in
  let pp = D.pp_charstring in
  let res = T.run_d_charstring ~gsubr_index ~lsubr_index data ~start ~charstring_length in
  assert_match ~pp ~message:"cff" TestCaseCff1.expected_operations res
