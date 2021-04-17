
let charstring_data =
  TestUtil.make_string_even [
  (* `lmroman10-regular.otf`, glyph ID = 50 (offset: 25799, length: 12) *)
    0x6f0a; 0x01a7; 0xdef7; 0x82cd; 0x0323; 0x0a0e;
  ]


let ( --> ) k v = (k, v)


let gsubrs = (264, [
  262 -->
    (* `lmroman10-regular.otf`, offset: 20600, length: 5 *)
    TestUtil.make_string_odd [0xf850; 0x80a4] 0x0b;
  176 -->
    (* `lmroman10-regular.otf`, offset: 19758, length: 7 *)
    TestUtil.make_string_odd [0xf833; 0xf70b; 0x1595] 0x0b;
  254 -->
    (* `lmroman10-regular.otf`, offset: 20534, length: 6 *)
    TestUtil.make_string_even [0x06a1; 0x8e8b; 0xa00b];
])


let lsubrs = (264, [
  79 -->
    (* `lmroman10-regular.otf`, offset: 63272, length: 10 *)
    TestUtil.make_string_even [0xf72f; 0x1df7; 0x6da0; 0xf742; 0xa10b];
  185 -->
    (* `lmroman10-regular.otf`, offset: 65457, length: 5 *)
    TestUtil.make_string_odd [0xfb11; 0xfb08] 0x0b;
  181 -->
    (* `lmroman10-regular.otf`, offset: 65426, length: 9 *)
    TestUtil.make_string_odd [0xf703; 0x9e1f; 0x49f7; 0x1915] 0x0b;
  3 -->
    (* `lmroman10-regular.otf`, offset: 60345, length: 71 *)
    TestUtil.make_string_odd [
      0xd01d; 0x838d; 0x8682; 0x8985; 0x8389; 0x1e24; 0x6831; 0x8b81;
      0x1b59; 0x63a9; 0xb074; 0x1f6d; 0xbb8b; 0xcdaf; 0x1af7; 0xabf7;
      0x271d; 0xee55; 0xecd9; 0x0a2f; 0x24fb; 0x11fb; 0x1af4; 0x2af7;
      0x07f7; 0x0eb8; 0xd50a; 0xfb81; 0x06f7; 0x2991; 0xdfa4; 0xad1b;
      0xf295; 0xfb1b; 0x641f;
    ] 0x0b;
])


let expected_operations =
  let open Otfed.Intermediate.Cff in
  [
    HStem(-11, 25, [(217, 21); (174, 22)]);
    VStem(28, 83, [(238, 66)]);
    RMoveTo(415, 119);
    VHCurveTo([(10, (-8, 2), -5); (-9, (-2, -6), -8)], Some(-2));
    HHCurveTo(Some(-103), [(-35, (-90, 0), -10)]);
    HVCurveTo([(-50, (-40, 30), 37)], Some(-23));
    VVCurveTo(Some(-30), [(48, (0, 66), 36)]);
    HLineTo[279];
    HVCurveTo([
      (22, (3, 0), 21);
      (99, (-54, 97), -125);
      (-116, (-92, -103), -125);
      (-134, (105, -97), 115);
      (122, (45, 111), 19)
    ], None);
    RMoveTo(-66, 133);
    HLineTo[-237];
    HHCurveTo((Some 149), [(6, (84, 25), 34)]);
    HVCurveTo([(103, (10, -135), -39)], None);
  ]


let expected_paths =
  let open Otfed.Value in
  [
    ((415, 119), [
      CubicCurveTo((415, 129), (407, 131), (402, 131));
      CubicCurveTo((393, 131), (391, 125), (389, 117));
      CubicCurveTo((354, 14), (264, 14), (254, 14));
      CubicCurveTo((204, 14), (164, 44), (141, 81));
      CubicCurveTo((111, 129), (111, 195), (111, 231));
      CubicLineTo(390, 231);
      CubicCurveTo((412, 231), (415, 231), (415, 252));
      CubicCurveTo((415, 351), (361, 448), (236, 448));
      CubicCurveTo((120, 448), (28, 345), (28, 220));
      CubicCurveTo((28, 86), (133, -11), (248, -11));
      CubicCurveTo((370, -11), (415, 100), (415, 119));
    ]);
    ((349, 252), [
      CubicLineTo(112, 252);
      CubicCurveTo((118, 401), (202, 426), (236, 426));
      CubicCurveTo((339, 426), (349, 291), (349, 252));
    ]);
  ]
