
module V = Otfed.Value


let data =
  TestUtil.make_string_even [
    (* `ipaexm.ttf`, glyph ID 1000 (offset: 520500, length: 608) *)
    0x0006; 0x0033; 0xff6d; 0x07ab; 0x0694; 0x001f; 0x0023; 0x0055;
    0x0059; 0x005d; 0x0061; 0x0125; 0x40ff; 0x1a16; 0x1402; 0x3a14;
    0x2f02; 0x3c2f; 0x3d02; 0x4124; 0x1e02; 0x043d; 0x0003; 0x0720;
    0x3f02; 0x433f; 0x4402; 0x4827; 0x260e; 0x0444; 0x4603; 0x4a11;
    0x1003; 0x464b; 0x034f; 0x210a; 0x0251; 0x4d52; 0x0236; 0x322d;
    0x2918; 0x050d; 0x1655; 0x540d; 0x0c09; 0x0806; 0x0d52; 0x3938;
    0x302f; 0x0359; 0x563e; 0x3d03; 0x5857; 0x403f; 0x035d; 0x5a45;
    0x4403; 0x5c5b; 0x4746; 0x0361; 0x5e4c; 0x4b03; 0x605f; 0x4e4d;
    0x0353; 0x5201; 0x1716; 0x011c; 0x1b15; 0x1403; 0x0100; 0x0123;
    0x2001; 0x2221; 0x010b; 0x0a01; 0x0e00; 0x0e38; 0x3002; 0x3e58;
    0x032b; 0x2f2d; 0x2b29; 0x0458; 0x5303; 0x0427; 0x261b; 0x1a04;
    0x0524; 0x0703; 0x1817; 0x0203; 0x0709; 0x031c; 0x0100; 0x0309;
    0x0b03; 0x1e14; 0x020b; 0x0d03; 0x5251; 0x4f4e; 0x4b4a; 0x4847;
    0x4443; 0x4140; 0x3d3c; 0x3a39; 0x3632; 0x120d; 0x343e; 0x1615;
    0x1110; 0x040d; 0x0d61; 0x605d; 0x5c59; 0x581e; 0x053e; 0x5f5e;
    0x5b5a; 0x5756; 0x5453; 0x1e07; 0x2408; 0x071e; 0x0109; 0x2120;
    0x0c0b; 0x1e03; 0x0d04; 0x044d; 0x4c46; 0x453f; 0x3e05; 0x5524;
    0x0123; 0x220a; 0x0903; 0x0e0d; 0x01b1; 0x0400; 0x2a2a; 0x102a;
    0x10c4; 0x2a2b; 0x2b2b; 0x2b2f; 0x2b2f; 0x2b2b; 0x2a10; 0x2a10;
    0x2a2b; 0x2b2b; 0x2b2b; 0x2b2b; 0x2b2b; 0x2b31; 0x3001; 0x3337;
    0x1615; 0x1407; 0x0711; 0x2335; 0x2315; 0x2311; 0x0607; 0x2700;
    0x1337; 0x2127; 0x2137; 0x1617; 0x0721; 0x0207; 0x1617; 0x1133;
    0x1125; 0x0607; 0x2712; 0x1316; 0x1514; 0x0706; 0x0721; 0x3613;
    0x1615; 0x1407; 0x0607; 0x3337; 0x1617; 0x0721; 0x1133; 0x3716;
    0x1707; 0x2111; 0x3337; 0x1617; 0x0721; 0x1133; 0x3716; 0x1707;
    0x2115; 0x2313; 0x1121; 0x1101; 0x1121; 0x1101; 0x1121; 0x1101;
    0xb0c6; 0x4c69; 0x0a29; 0x71e9; 0x7550; 0x7d29; 0x010b; 0x5a04;
    0xfed9; 0x1102; 0x2967; 0x4f34; 0x17fe; 0xc13e; 0x5c11; 0x06e9;
    0x015c; 0x555b; 0x2bf5; 0x79bf; 0x5a2f; 0x4001; 0x0e4e; 0x33bd;
    0x544a; 0x60e5; 0x604b; 0x3214; 0xfe7f; 0x9956; 0x4031; 0x14fe;
    0xb499; 0x5b3d; 0x3316; 0xfeb2; 0xe767; 0x3e38; 0x16fc; 0xc173;
    0x7301; 0x1efe; 0xe201; 0x1efe; 0xe201; 0x1e03; 0x8f5e; 0x591b;
    0x0a07; 0x1afc; 0x9d98; 0xd903; 0x3596; 0xae1f; 0x01b7; 0x0228;
    0x1933; 0x8550; 0x4325; 0xfed6; 0xe007; 0x45fd; 0x5a02; 0xa650;
    0x8e72; 0x2101; 0x7b02; 0x4c2e; 0x2a1c; 0x0fb1; 0x9fb7; 0x010e;
    0x352e; 0x1d03; 0xad95; 0x8141; 0x4a2a; 0xfebd; 0x7b40; 0x4529;
    0xfeaa; 0x793b; 0x462c; 0xfe94; 0x8541; 0x4c2b; 0x8105; 0x20fe;
    0xbd01; 0x43fe; 0x8afe; 0xaa01; 0x56fe; 0x76fe; 0x9401; 0x6c00;
  ]


let expected =
  let convert (on_curve, x_coord, y_coord) = V.Ttf.{ on_curve; point = (x_coord, y_coord) } in
  let description =
    V.Ttf.SimpleGlyph[
      List.map convert [
        (true, 432, 911);
        (true, 630, 911);
        (true, 706, 1005);
        (false, 811, 916);
        (true, 811, 889);
        (false, 811, 879);
        (true, 801, 872);
        (true, 760, 846);
        (true, 760, -21);
        (true, 647, -21);
        (true, 647, 131);
        (true, 414, 131);
        (true, 414, -86);
        (true, 297, -86);
        (true, 297, 735);
        (false, 217, 585);
        (true, 92, 411);
        (true, 51, 442);
        (false, 318, 881);
        (true, 408, 1433);
        (true, 412, 1458);
        (true, 117, 1458);
        (true, 100, 1509);
        (true, 653, 1509);
        (true, 756, 1642);
        (false, 835, 1562);
        (true, 887, 1495);
        (true, 864, 1458);
        (true, 545, 1458);
        (false, 483, 1160);
        (true, 391, 936);
        (false, 408, 929);
      ];
      List.map convert [
        (true, 414, 860);
        (true, 414, 182);
        (true, 647, 182);
        (true, 647, 860);
      ];
      List.map convert [
        (true, 995, 940);
        (false, 910, 798);
        (true, 819, 684);
        (true, 776, 717);
        (false, 1021, 1096);
        (true, 1142, 1684);
        (false, 1333, 1638);
        (true, 1333, 1596);
        (false, 1333, 1568);
        (true, 1243, 1553);
        (false, 1196, 1376);
        (true, 1132, 1217);
        (true, 1402, 1217);
        (false, 1480, 1400);
        (true, 1531, 1670);
        (false, 1720, 1617);
        (true, 1720, 1571);
        (false, 1720, 1542);
        (true, 1636, 1539);
        (false, 1562, 1366);
        (true, 1466, 1217);
        (true, 1695, 1217);
        (true, 1791, 1346);
        (false, 1866, 1281);
        (true, 1916, 1207);
        (true, 1896, 1165);
        (true, 1511, 1165);
        (true, 1511, 842);
        (true, 1664, 842);
        (true, 1750, 965);
        (false, 1814, 901);
        (true, 1863, 832);
        (true, 1843, 791);
        (true, 1511, 791);
        (true, 1511, 449);
        (true, 1664, 449);
        (true, 1755, 570);
        (false, 1816, 511);
        (true, 1867, 441);
        (true, 1845, 397);
        (true, 1511, 397);
        (true, 1511, 33);
        (true, 1742, 33);
        (true, 1845, 166);
        (false, 1907, 101);
        (true, 1963, 25);
        (true, 1941, -18);
        (true, 1110, -18);
        (true, 1110, -147);
        (true, 995, -147);
      ];
      List.map convert [
        (true, 1110, 1165);
        (true, 1110, 842);
        (true, 1396, 842);
        (true, 1396, 1165);
      ];
      List.map convert [
        (true, 1110, 791);
        (true, 1110, 449);
        (true, 1396, 449);
        (true, 1396, 791);
      ];
      List.map convert [
        (true, 1110, 397);
        (true, 1110, 33);
        (true, 1396, 33);
        (true, 1396, 397);
      ];
    ]
  in
  let bounding_box = V.{ x_min = 51; y_min = -147; x_max = 1963; y_max = 1684 } in
  V.Ttf.{ description; bounding_box }
