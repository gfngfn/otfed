
module V = Otfed.Value
module D = Otfed.Decode
module E = Otfed.Encode
module DT = Otfed.Decode.ForTest
module ET = Otfed.Encode.ForTest


let assert_match ~pp ~pp_error ~message:msg expected = function
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
      Format.printf "%a\n" pp_error e;
      exit 1


let pp_xxd ppf s =
  let pp_single ppf ch =
    Format.fprintf ppf "%02x" (Char.code ch)
  in
  let chars = Core_kernel.String.to_list_rev s |> List.rev in
  Format.fprintf ppf "%a" (Format.pp_print_list pp_single) chars


let () =
  let cases =
    [
      (0b01_01_01_01_01_00_00_00, 2, 5, [1; 1; 1; 1; 1]);
      (0b00_01_10_11_00_00_00_00, 2, 4, [0; 1; -2; -1]);
      (0b0101_0101_0001_0000, 4, 3, [5; 5; 1]);
      (0b0101_0101_0001_0000, 4, 4, [5; 5; 1; 0]);
      (0b1111_1101_0001_0000, 4, 4, [-1; -3; 1; 0]);
      (0b11111111_00010000, 8, 2, [-1; 16]);
    ]
  in
  let pp ppf ds =
    let pp_sep ppf () = Format.fprintf ppf ", " in
    Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep Format.pp_print_int) ds
  in
  cases |> List.iter (fun (data, unit_size, repeat, expected) ->
    let got = DT.chop_two_bytes ~data ~unit_size ~repeat in
    assert_match
      ~pp
      ~pp_error:D.Error.pp
      ~message:"chop_two_bytes"
      expected
      (Ok(got))
  )


module DecOp = DT.DecodeOperation
module EncOp = ET.EncodeOperation


let () =
  let cases =
    [
      (TestUtil.make_string_even [0xffec], -20);
    ]
  in
  cases |> List.iter (fun (input, expected) ->
    let res = DecOp.d_int16 |> DT.run input in
    assert_match
      ~pp:Format.pp_print_int
      ~pp_error:D.Error.pp
      ~message:"d_int16"
      expected
      res
  );
  cases |> List.iter (fun (expected, input) ->
    let res = EncOp.e_int16 input |> ET.run in
    assert_match
      ~pp:pp_xxd
      ~pp_error:E.Error.pp
      ~message:"e_int16"
      expected
      res
  )


let () =
  let pp ppf V.{ description = descr; bounding_box = bbox } =
    Format.fprintf ppf "(%a, %a)" V.pp_ttf_glyph_description descr V.pp_bounding_box bbox
  in
  let res = DT.run TestCaseGlyf1.data DT.d_glyf in
  assert_match
    ~pp
    ~pp_error:D.Error.pp
    ~message:"glyf"
    TestCaseGlyf1.expected
    res


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
  let res = DT.run_d_charstring ~gsubr_index ~lsubr_index data ~start ~charstring_length in
  assert_match
    ~pp
    ~pp_error:D.Error.pp
    ~message:"cff"
    TestCaseCff1.expected_operations
    res
