
open TestUtil

open Otfed__Basic
module DecodeOperation = Otfed__DecodeOperation
module EncodeOperation = Otfed__EncodeOperation
module DecodeHead = Otfed__DecodeHead
module EncodeHead = Otfed__EncodeHead
module DecodeTtf = Otfed__DecodeTtf
module EncodeTtf = Otfed__EncodeTtf
module DecodeCff = Otfed__DecodeCff
module DecodeError = Otfed__DecodeError
module EncodeError = Otfed__EncodeError
module Value = Otfed__Value
module Intermediate = Otfed__Intermediate


(** Tests for `DecodeOperation.d_int16` and `EncodeOperation.e_int16` *)
let d_int16_and_e_int16_tests () =
  let cases =
    [
      (make_string_even [0xffec], -20);
    ]
  in
  cases |> List.iter (fun (input, expected) ->
    let res = DecodeOperation.d_int16 |> run_decoder input in
    Alcotest.(check (decoding int)) "d_int16" (Ok(expected)) res
  );
  cases |> List.iter (fun (expected, input) ->
    let res = EncodeOperation.e_int16 input |> run_encoder in
    Alcotest.(check encoding) "e_int16" (Ok(expected)) res
  )


(** Tests for `DecodeHead.d_head` *)
let d_head_tests () =
  let got = DecodeHead.d_head Intermediate.LongLocFormat |> run_decoder TestCaseHead1.marshaled in
  let expected = Ok(TestCaseHead1.unmarshaled) in
  Alcotest.(check (decoding (of_pp Intermediate.Head.pp))) "d_head" expected got


(** Tests for `EncodeHead.e_head` *)
let e_head_tests () =
  let got = EncodeHead.e_head TestCaseHead1.unmarshaled |> run_encoder in
  let expected = Ok(TestCaseHead1.marshaled) in
  Alcotest.(check encoding) "e_head" expected got


(** Tests for `DecodeTtf.d_glyph` *)
let d_glyph_tests () =
  let got = DecodeTtf.d_glyph |> run_decoder TestCaseGlyf1.marshaled in
  let expected = Ok(TestCaseGlyf1.unmarshaled) in
  Alcotest.(check (decoding (of_pp Value.Ttf.pp_glyph_info))) "d_glyph" expected got


(** Tests for `DecodeTtf.e_glyph` *)
let e_glyph_tests () =
  let got = EncodeTtf.e_glyph TestCaseGlyf1.unmarshaled |> run_encoder in
  let expected = Ok(TestCaseGlyf1.marshaled) in
  Alcotest.(check encoding) "e_glyph" expected got


let run_d_charstring ~gsubr_index ~lsubr_index data ~start ~charstring_length =
  let cstate = DecodeCff.initial_charstring_state charstring_length in
  let dec =
    let open DecodeOperation in
    DecodeCff.d_charstring { gsubr_index; lsubr_index } cstate >>= fun (_, opacc) ->
    return @@ Alist.to_list opacc
  in
  dec |> DecodeOperation.run { data = data; max = String.length data } start


(** Tests for `DecodeCff.d_charstring` and `DecodeCff.path_of_charstring` *)
let d_charstring_tests () =
  let (gsize, gkeyval) = TestCaseCff1.gsubrs in
  let gsubr_index = Array.make gsize (Intermediate.Cff.CharStringData(0, 0)) in
  let (lsize, lkeyval) = TestCaseCff1.lsubrs in
  let lsubr_index = Array.make lsize (Intermediate.Cff.CharStringData(0, 0)) in
  let (start, data, charstring_length) =
    let buf = Buffer.create 1024 in
    let start =
      gkeyval |> List.fold_left (fun start (i, s) ->
        let len = String.length s in
        Format.printf "| Write: global, biased = %d, offset = %d, length = %d\n" i start len;
        gsubr_index.(i) <- Intermediate.Cff.CharStringData(start, len);
        Buffer.add_string buf s;
        start + len
      ) 0
    in
    let start =
      lkeyval |> List.fold_left (fun start (i, s) ->
        let len = String.length s in
        Format.printf "| Write: local, biased = %d, offset = %d, length = %d\n" i start len;
        lsubr_index.(i) <- Intermediate.Cff.CharStringData(start, len);
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
  let res1 = run_d_charstring ~gsubr_index ~lsubr_index data ~start ~charstring_length in
  Alcotest.(check (decoding (of_pp Intermediate.Cff.pp_charstring))) "cff"
    (Ok(TestCaseCff1.expected_operations)) res1;
  res1 |> get_or_fail ~pp_error:DecodeError.pp (fun charstring ->
    let res2 = DecodeCff.path_of_charstring charstring in
    Alcotest.(check (decoding (list (of_pp Value.pp_cubic_path)))) "cff cubic path"
      (Ok(TestCaseCff1.expected_paths)) res2
  )


let () =
  let open Alcotest in
  run "Otfed" [
    ("DecodeOperation, EncodeOperation", [
      test_case "d_int16, e_int16" `Quick d_int16_and_e_int16_tests;
    ]);
    ("DecodeHead", [
      test_case "d_head" `Quick d_head_tests;
    ]);
    ("EncodeHead", [
      test_case "e_head" `Quick e_head_tests;
    ]);
    ("DecodeTtf", [
      test_case "d_glyph" `Quick d_glyph_tests;
    ]);
    ("EncodeTtf", [
      test_case "e_glyph" `Quick e_glyph_tests;
    ]);
    ("DecodeCff", [
      test_case "d_charstring" `Quick d_charstring_tests;
    ]);
  ]
