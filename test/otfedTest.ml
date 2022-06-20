
open TestUtil

open Otfed__Basic
module DecodeOperation = Otfed__DecodeOperation
module EncodeOperation = Otfed__EncodeOperation
module DecodeTtf = Otfed__DecodeTtf
module DecodeCff = Otfed__DecodeCff
module DecodeError = Otfed__DecodeError
module EncodeError = Otfed__EncodeError
module Value = Otfed__Value
module Intermediate = Otfed__Intermediate


let run_decoder s d =
  DecodeOperation.run { data = s; max = String.length s } 0 d


let run_d_charstring ~gsubr_index ~lsubr_index data ~start ~charstring_length =
  let cstate = DecodeCff.initial_charstring_state charstring_length in
  let dec =
    let open DecodeOperation in
    DecodeCff.d_charstring { gsubr_index; lsubr_index } cstate >>= fun (_, opacc) ->
    return @@ Alist.to_list opacc
  in
  dec |> DecodeOperation.run { data = data; max = String.length data } start


let run_encoder enc =
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, _) ->
  return contents


let decoding testable_ok =
  Alcotest.(result testable_ok (of_pp DecodeError.pp))


let encoding testable_ok =
  Alcotest.(result testable_ok (of_pp EncodeError.pp))


(** Tests for `DecodeOperation.chop_two_bytes` *)
let chop_two_bytes_tests () =
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
  cases |> List.iter (fun (data, unit_size, repeat, expected) ->
    let got = DecodeOperation.ForTest.chop_two_bytes ~data ~unit_size ~repeat in
    Alcotest.(check (list int)) "chop_two_bytes" expected got
  )


(** Tests for `DecodeOperation.d_int16` and `EncodeOperation.e_int16` *)
let d_int16_and_e_int16_tests () =
  let cases =
    [
      (make_string_even [0xffec], -20);
    ]
  in
  cases |> List.iter (fun (input, expected) ->
    let res = DecodeOperation.d_int16 |> run_decoder input in
    Alcotest.(check (decoding int)) "d_int16"
      (Ok(expected)) res
  );
  cases |> List.iter (fun (expected, input) ->
    let res = EncodeOperation.e_int16 input |> run_encoder in
    Alcotest.(check (encoding (of_pp pp_xxd))) "e_int16"
      (Ok(expected)) res
  )


(** Tests for `DecodeTtf.d_glyf` *)
let d_glyf_tests () =
  let res = DecodeTtf.d_glyf |> run_decoder TestCaseGlyf1.data in
  Alcotest.(check (decoding (of_pp Value.Ttf.pp_glyph_info))) "glyf"
    (Ok(TestCaseGlyf1.expected)) res


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
    ("Basic", [
      test_case "chop_two_bytes" `Quick chop_two_bytes_tests;
      test_case "d_int16, e_int16" `Quick d_int16_and_e_int16_tests;
    ]);
    ("Glyph", [
      test_case "d_glyf" `Quick d_glyf_tests;
      test_case "d_charstring" `Quick d_charstring_tests;
    ]);
  ]
