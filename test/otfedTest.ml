
open TestUtil

open Otfed__Basic
module DecodeBasic = Otfed__DecodeBasic
module DecodeOperation = Otfed__DecodeOperation
module EncodeOperation = Otfed__EncodeOperation
module Encode = Otfed__Encode
module DecodeHead = Otfed__DecodeHead
module EncodeHead = Otfed__EncodeHead
module DecodeHhea = Otfed__DecodeHhea
module EncodeHhea = Otfed__EncodeHhea
module DecodeOs2 = Otfed__DecodeOs2
module EncodeOs2 = Otfed__EncodeOs2
module DecodeTtfMaxp = Otfed__DecodeTtfMaxp
module EncodeTtfMaxp = Otfed__EncodeTtfMaxp
module DecodeCffMaxp = Otfed__DecodeCffMaxp
module EncodeCffMaxp = Otfed__EncodeCffMaxp
module DecodeHmtx = Otfed__DecodeHmtx
module EncodeHmtx = Otfed__EncodeHmtx
module DecodeCmap = Otfed__DecodeCmap
module EncodeCmap = Otfed__EncodeCmap
module DecodeName = Otfed__DecodeName
module EncodeName = Otfed__EncodeName
module DecodePost = Otfed__DecodePost
module EncodePost = Otfed__EncodePost
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


(** Tests for `DecodeHhea.d_hhea` *)
let d_hhea_tests () =
  let got = DecodeHhea.d_hhea |> run_decoder TestCaseHhea1.marshaled in
  let expected = Ok(TestCaseHhea1.unmarshaled) in
  Alcotest.(check (decoding (of_pp Intermediate.Hhea.pp))) "d_hhea" expected got


(** Tests for `EncodeHhea.e_hhea` *)
let e_hhea_tests () =
  let number_of_h_metrics = TestCaseHhea1.number_of_h_metrics in
  let got = EncodeHhea.e_hhea ~number_of_h_metrics TestCaseHhea1.unmarshaled |> run_encoder in
  let expected = Ok(TestCaseHhea1.marshaled) in
  Alcotest.(check encoding) "e_hhea" expected got


(** Tests for `DecodeOs2.d_os2` *)
let d_os2_tests () =
  let got = DecodeOs2.d_os2 |> run_decoder TestCaseOs21.marshaled in
  let expected = Ok(TestCaseOs21.unmarshaled) in
  Alcotest.(check (decoding (of_pp Intermediate.Os2.pp))) "d_os2" expected got


(** Tests for `EncodeOs2.e_os2` *)
let e_os2_tests () =
  let got = EncodeOs2.e_os2 TestCaseOs21.unmarshaled |> run_encoder in
  let expected = Ok(TestCaseOs21.marshaled) in
  Alcotest.(check encoding) "e_os2" expected got


(** Tests for `DecodeTtfMaxp.d_maxp` *)
let d_ttf_maxp_tests () =
  let got = DecodeTtfMaxp.d_maxp |> run_decoder TestCaseTtfMaxp1.marshaled in
  let expected = Ok(TestCaseTtfMaxp1.unmarshaled) in
  Alcotest.(check (decoding (of_pp Intermediate.Ttf.Maxp.pp))) "d_maxp" expected got


(** Tests for `EncodeTtfMaxp.e_maxp` *)
let e_ttf_maxp_tests () =
  let got = EncodeTtfMaxp.e_maxp TestCaseTtfMaxp1.unmarshaled |> run_encoder in
  let expected = Ok(TestCaseTtfMaxp1.marshaled) in
  Alcotest.(check encoding) "e_maxp" expected got


(** Tests for `DecodeCffMaxp.d_maxp` *)
let d_cff_maxp_tests () =
  let got = DecodeCffMaxp.d_maxp |> run_decoder TestCaseCffMaxp1.marshaled in
  let expected = Ok(TestCaseCffMaxp1.unmarshaled) in
  Alcotest.(check (decoding (of_pp Intermediate.Cff.Maxp.pp))) "d_maxp" expected got


(** Tests for `EncodeCffMaxp.e_maxp` *)
let e_cff_maxp_tests () =
  let got = EncodeCffMaxp.e_maxp TestCaseCffMaxp1.unmarshaled |> run_encoder in
  let expected = Ok(TestCaseCffMaxp1.marshaled) in
  Alcotest.(check encoding) "e_maxp" expected got


(** Tests for `DecodeHmtx.access` *)
let access_hmtx_tests () =
  let hmtx =
    let data = TestCaseHmtx1.marshaled in
    let length = String.length data in
    let core = DecodeBasic.{ data = data; max = length; } in
    DecodeHmtx.make_scheme core 0 length
      {
        num_glyphs    = TestCaseHmtx1.number_of_glyphs;
        num_h_metrics = TestCaseHmtx1.number_of_h_metrics;
      }
  in
  TestCaseHmtx1.test_cases |> List.iteri (fun index (gid, pair_opt) ->
    let title = Printf.sprintf "access_hmtx (%d)" index in
    let got = DecodeHmtx.access hmtx gid in
    let expected = Ok(pair_opt) in
    Alcotest.(check (decoding (option (pair int int)))) title expected got
  )


(** Tests for `EncodeHmtx.make_exact` *)
let make_hmtx_tests () =
  let got =
    EncodeHmtx.make_exact
      TestCaseHmtx1.unmarshaled_long_hor_metrics
      TestCaseHmtx1.unmarshaled_left_side_bearings
    |> Result.map Encode.get_contents
  in
  let expected = Ok(TestCaseHmtx1.marshaled) in
  Alcotest.(check encoding) "make_hmtx" expected got


let d_cmap_subtable_to_list =
  let open DecodeOperation in
  DecodeCmap.d_cmap_subtable (fun acc segment -> Alist.extend acc segment) Alist.empty >>= fun acc ->
  return (Alist.to_list acc)


(** Tests for `DecodeCmap.d_cmap_subtable` *)
let d_cmap_subtable_tests () =
  begin
    let got = d_cmap_subtable_to_list |> run_decoder TestCaseCmap1.marshaled in
    let expected = Ok(TestCaseCmap1.unmarshaled) in
    Alcotest.(check (decoding (list (of_pp DecodeCmap.pp_segment)))) "d_cmap_subtable (1: Format 4)" expected got
  end;
  begin
    let got = d_cmap_subtable_to_list |> run_decoder TestCaseCmap2.marshaled in
    let expected = Ok(TestCaseCmap2.unmarshaled) in
    Alcotest.(check (decoding (list (of_pp DecodeCmap.pp_segment)))) "d_cmap_subtable (2: Format 12)" expected got
  end


(** Tests for `EncodeCmap.e_cmap_mapping` *)
let e_cmap_mapping_tests () =
  let input = TestCaseCmap2.unmarshaled in
  let cmap_mapping =
    input |> List.fold_left (fun cmap_mapping segment ->
      match segment with
      | DecodeCmap.Incremental(start, last, gid) ->
          cmap_mapping |> Value.Cmap.Mapping.add_incremental_range ~start ~last ~gid

      | DecodeCmap.Constant(start, last, gid) ->
          cmap_mapping |> Value.Cmap.Mapping.add_constant_range ~start ~last ~gid
    ) Value.Cmap.Mapping.empty
  in
  let got = EncodeCmap.e_cmap_mapping cmap_mapping |> run_encoder in
  let expected = Ok(TestCaseCmap2.marshaled) in
  Alcotest.(check encoding) "e_cmap_mapping" expected got


(** Tests for `DecodeName.d_name` *)
let d_name_tests () =
  let got = DecodeName.d_name |> run_decoder TestCaseName1.marshaled in
  let expected = Ok(TestCaseName1.unmarshaled) in
  Alcotest.(check (decoding (of_pp Value.Name.pp))) "d_name" expected got


(** Tests for `EncodeName.encode_name` *)
let encode_name_tests () =
  let got = EncodeName.encode_name TestCaseName1.unmarshaled in
  match got with
  | Ok(s) ->
      let got = DecodeName.d_name |> run_decoder s in
      let expected = Ok(TestCaseName1.unmarshaled) in
      Alcotest.(check (decoding (of_pp Value.Name.pp))) "encode_name -> d_name" expected got

  | Error(e) ->
      Alcotest.failf "%a" EncodeError.pp e


(** Tests for `DecodePost.d_post` *)
let d_post_tests () =
  begin
    let input = TestCasePost1.marshaled in
    let num_glyphs = TestCasePost1.num_glyphs in
    let got = DecodePost.d_post ~num_glyphs ~length:(String.length input) |> run_decoder input in
    let expected = Ok(TestCasePost1.unmarshaled) in
    Alcotest.(check (decoding (of_pp Value.Post.pp))) "d_post (1: Version 3)" expected got
  end;
  begin
    let input = TestCasePost2.marshaled in
    let num_glyphs = TestCasePost2.num_glyphs in
    let got = DecodePost.d_post ~num_glyphs ~length:(String.length input) |> run_decoder input in
    let expected = Ok(TestCasePost2.unmarshaled) in
    Alcotest.(check (decoding (of_pp Value.Post.pp))) "d_post (2: Version 2)" expected got
  end


(** Tests for `EncodePost.e_post` *)
let e_post_tests () =
  begin
    let num_glyphs = TestCasePost1.num_glyphs in
    let got = EncodePost.e_post ~num_glyphs TestCasePost1.unmarshaled |> run_encoder in
    let expect = Ok(TestCasePost1.marshaled) in
    Alcotest.(check encoding) "e_post (1: Version 3)" expect got
  end;
  begin
    let num_glyphs = TestCasePost2.num_glyphs in
    let got = EncodePost.e_post ~num_glyphs TestCasePost2.unmarshaled |> run_encoder in
    let expect = Ok(TestCasePost2.marshaled) in
    Alcotest.(check encoding) "e_post (2: Version 2)" expect got
  end


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
    ("DecodeHhea", [
      test_case "d_hhea" `Quick d_hhea_tests;
    ]);
    ("EncodeHhea", [
      test_case "e_hhea" `Quick e_hhea_tests;
    ]);
    ("DecodeOs2", [
      test_case "d_os2" `Quick d_os2_tests;
    ]);
    ("EncodeOs2", [
      test_case "e_os2" `Quick e_os2_tests;
    ]);
    ("DecodeTtfMaxp", [
      test_case "d_maxp" `Quick d_ttf_maxp_tests;
    ]);
    ("EncodeTtfMaxp", [
      test_case "e_maxp" `Quick e_ttf_maxp_tests;
    ]);
    ("DecodeCffMaxp", [
      test_case "d_maxp" `Quick d_cff_maxp_tests;
    ]);
    ("EncodeCffMaxp", [
      test_case "e_maxp" `Quick e_cff_maxp_tests;
    ]);
    ("DecodeHmtx", [
      test_case "access_hmtx" `Quick access_hmtx_tests;
    ]);
    ("EncodeHmtx", [
      test_case "make_hmtx" `Quick make_hmtx_tests;
    ]);
    ("DecodeCmap", [
      test_case "d_cmap_subtable" `Quick d_cmap_subtable_tests;
    ]);
    ("EncodeCmap", [
      test_case "e_cmap_mapping" `Quick e_cmap_mapping_tests;
    ]);
    ("DecodeName", [
      test_case "d_name" `Quick d_name_tests;
    ]);
    ("EncodeName", [
      test_case "encode_name" `Quick encode_name_tests;
    ]);
    ("DecodePost", [
      test_case "d_post" `Quick d_post_tests;
    ]);
    ("EncodePost", [
      test_case "e_post" `Quick e_post_tests;
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
