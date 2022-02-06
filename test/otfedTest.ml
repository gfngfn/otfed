
open TestUtil

module V = Otfed.Value
module I = Otfed.Intermediate
module D = Otfed.Decode
module E = Otfed.Encode
module DT = Otfed.Decode.ForTest
module ET = Otfed.Encode.ForTest
module DecOp = DT.DecodeOperation
module EncOp = ET.EncodeOperation


(** Tests for `DecodeOperation.chop_two_bytes` (via `DT.chop_two_bytes`) *)
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
    Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep Format.pp_print_int) ds
  in
  cases |> List.iter (fun (data, unit_size, repeat, expected) ->
    let got = DT.chop_two_bytes ~data ~unit_size ~repeat in
    ignore @@ assert_equal
      ~pp
      ~pp_error:D.Error.pp
      ~message:"chop_two_bytes"
      expected
      (Ok(got))
  )


(** Tests for `DecodeOperation.d_int16` (via `DecOp.d_int16`)
    and `EncodeOperation.e_int16` (via `EncOp.e_int16`) *)
let () =
  let cases =
    [
      (make_string_even [0xffec], -20);
    ]
  in
  cases |> List.iter (fun (input, expected) ->
    let res = DecOp.d_int16 |> DT.run input in
    ignore @@ assert_equal
      ~pp:Format.pp_print_int
      ~pp_error:D.Error.pp
      ~message:"d_int16"
      expected
      res
  );
  cases |> List.iter (fun (expected, input) ->
    let res = EncOp.e_int16 input |> ET.run in
    ignore @@ assert_equal
      ~pp:pp_xxd
      ~pp_error:E.Error.pp
      ~message:"e_int16"
      expected
      res
  )


(** Tests for `DecodeTtf.d_glyf` (via `DT.d_glyf`) *)
let () =
  let res = DT.run TestCaseGlyf1.data DT.d_glyf in
  ignore @@ assert_equal
    ~pp:V.Ttf.pp_glyph_info
    ~pp_error:D.Error.pp
    ~message:"glyf"
    TestCaseGlyf1.expected
    res


(** Tests for `DecodeCff.d_charstring` and `DecodeCff.path_of_charstring` (via `DT.run_d_charstring`) *)
let () =
  let (gsize, gkeyval) = TestCaseCff1.gsubrs in
  let gsubr_index = Array.make gsize (I.Cff.CharStringData(0, 0)) in
  let (lsize, lkeyval) = TestCaseCff1.lsubrs in
  let lsubr_index = Array.make lsize (I.Cff.CharStringData(0, 0)) in
  let (start, data, charstring_length) =
    let buf = Buffer.create 1024 in
    let start =
      gkeyval |> List.fold_left (fun start (i, s) ->
        let len = String.length s in
        Format.printf "| Write: global, biased = %d, offset = %d, length = %d\n" i start len;
        gsubr_index.(i) <- I.Cff.CharStringData(start, len);
        Buffer.add_string buf s;
        start + len
      ) 0
    in
    let start =
      lkeyval |> List.fold_left (fun start (i, s) ->
        let len = String.length s in
        Format.printf "| Write: local, biased = %d, offset = %d, length = %d\n" i start len;
        lsubr_index.(i) <- I.Cff.CharStringData(start, len);
        Buffer.add_string buf s;
        start + len
      ) start
    in
    let charstring_length = String.length TestCaseCff1.charstring_data in
    Format.printf "| start: %d, charstring_length: %d\n" start charstring_length;
    Buffer.add_string buf TestCaseCff1.charstring_data;
    let data = Buffer.contents buf in
    (start, data, charstring_length)
  in
  let res1 = DT.run_d_charstring ~gsubr_index ~lsubr_index data ~start ~charstring_length in
  let charstring =
    assert_equal
      ~pp:I.Cff.pp_charstring
      ~pp_error:D.Error.pp
      ~message:"cff"
      TestCaseCff1.expected_operations
      res1
  in
  let res2 = D.Cff.path_of_charstring charstring in
  ignore @@ assert_equal
    ~pp:(Format.pp_print_list ~pp_sep Otfed.Value.pp_cubic_path)
    ~pp_error:D.Error.pp
    ~message:"cff cubic path"
    TestCaseCff1.expected_paths
    res2
