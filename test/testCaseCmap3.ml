
module DecodeCmap = Otfed__DecodeCmap


let marshaled =
  TestUtil.make_string_odd [
    (* `ipaexm.ttf` (offset: 79672, length: 2179); a cmap subtable of Format 14 *)
    0x000e; 0x0000; 0x0883; 0x0000; 0x0007; 0x00fe; 0x0000; 0x0000;
    0x5700; 0x0000; 0x7700; 0xfe01; 0x0000; 0x0000; 0x0000; 0x021a;
    0x0e01; 0x0000; 0x0002; 0x2d00; 0x0002; 0x590e; 0x0101; 0x0000;
    0x0587; 0x0000; 0x07e7; 0x0e01; 0x0200; 0x0008; 0x2700; 0x0008;
    0x530e; 0x0103; 0x0000; 0x0866; 0x0000; 0x0872; 0x0e01; 0x0500;
    0x0008; 0x7b00; 0x0000; 0x0000; 0x0000; 0x0700; 0x51a4; 0x0000;
    0x5c60; 0x0000; 0x6753; 0x0000; 0x6adb; 0x0000; 0x6eba; 0x0000;
    0x6f23; 0x0000; 0x7149; 0x0000; 0x0000; 0x5300; 0x4fae; 0x212b;
    0x0050; 0xe721; 0x1700; 0x514d; 0x2131; 0x0051; 0xde2f; 0x9e00;
    0x52c9; 0x212d; 0x0052; 0xe421; 0x0300; 0x5351; 0x2127; 0x0055;
    0x9d1c; 0xb900; 0x5606; 0x211d; 0x0056; 0x6820; 0xfe00; 0x5840;
    0x212c; 0x0058; 0x5a1c; 0xf900; 0x58a8; 0x212f; 0x005c; 0x6421;
    0x1800; 0x5c6e; 0x267b; 0x005e; 0xca2f; 0x9a00; 0x6094; 0x20f7;
    0x0061; 0x6820; 0xf900; 0x618e; 0x211a; 0x0061; 0xf22f; 0xaa00;
    0x654f; 0x212a; 0x0065; 0xe220; 0xff00; 0x6674; 0x1eb0; 0x0066;
    0x9121; 0x1100; 0x6717; 0x2f9c; 0x0068; 0x8521; 0x2400; 0x6b04;
    0x2134; 0x006b; 0xba21; 0x0900; 0x6d77; 0x20f8; 0x006e; 0x1a1c;
    0xdb00; 0x6f22; 0x20fd; 0x0071; 0x6e21; 0x0c00; 0x722b; 0x24ec;
    0x0073; 0x2a1e; 0xf100; 0x7422; 0x1cef; 0x0076; 0xca1f; 0x0700;
    0x7891; 0x2128; 0x0079; 0x3c1f; 0x0f00; 0x793e; 0x210d; 0x0079;
    0x4821; 0x0000; 0x7949; 0x210a; 0x0079; 0x5021; 0x3300; 0x7956;
    0x2116; 0x0079; 0x5d21; 0x1000; 0x795e; 0x1f10; 0x0079; 0x651f;
    0x1100; 0x798d; 0x20f6; 0x0079; 0x8e21; 0x2100; 0x798f; 0x1f13;
    0x007a; 0x4021; 0x0800; 0x7a81; 0x2122; 0x007b; 0xc021; 0x1500;
    0x7cbe; 0x1f1a; 0x007e; 0x092c; 0x7400; 0x7e41; 0x2125; 0x007f;
    0x7221; 0x1200; 0x7fbd; 0x1f22; 0x0080; 0x0521; 0x0e00; 0x81ed;
    0x210f; 0x0082; 0x7921; 0x7b00; 0x8457; 0x211e; 0x0086; 0x122f;
    0xab00; 0x865c; 0x2136; 0x0089; 0x1020; 0xfc00; 0x8996; 0x210b;
    0x008a; 0xf81f; 0x3900; 0x8b01; 0x20f2; 0x008b; 0x3921; 0x0400;
    0x8cd3; 0x2129; 0x008d; 0x0821; 0x1b00; 0x8fb6; 0x24ed; 0x0090;
    0x381f; 0x4300; 0x90fd; 0x1f46; 0x0096; 0x8621; 0x3500; 0x96e3;
    0x2123; 0x0097; 0x561f; 0x1700; 0x97ff; 0x2102; 0x0098; 0x3b1d;
    0x1900; 0x985e; 0x2138; 0x0098; 0xef1f; 0x8300; 0x98fc; 0x1f84;
    0x0099; 0x281f; 0x8600; 0x9db4; 0x1f93; 0x0000; 0x0003; 0x007d;
    0xf421; 0x3b00; 0x8279; 0x217a; 0x0090; 0x3820; 0xf100; 0x0000;
    0x0a00; 0x5307; 0x0000; 0x55a9; 0x0000; 0x5bdb; 0x0000; 0x61f2;
    0x0000; 0x646f; 0x0000; 0x6adb; 0x0000; 0x6f23; 0x0000; 0x6ff9;
    0x0000; 0x7149; 0x0000; 0x8612; 0x0000; 0x0000; 0xa200; 0x5026;
    0x065d; 0x0050; 0xc505; 0xdd00; 0x5132; 0x0dfb; 0x0051; 0x4e0b;
    0x5600; 0x51a4; 0x0f9a; 0x0053; 0xa903; 0xf100; 0x53c9; 0x073b;
    0x0053; 0xdb0c; 0x6a00; 0x53df; 0x1002; 0x0054; 0xac10; 0x2600;
    0x54e8; 0x08a3; 0x0055; 0xb006; 0x0200; 0x5632; 0x1067; 0x0056;
    0x4203; 0xf500; 0x564c; 0x09d1; 0x0056; 0xc010; 0x7c00; 0x5835;
    0x0b58; 0x0059; 0x0628; 0xf300; 0x5a29; 0x0d40; 0x005c; 0x5106;
    0x0b00; 0x5c60; 0x0b5b; 0x005d; 0xf706; 0xd300; 0x5e96; 0x0d5f;
    0x005e; 0xdf0c; 0xc800; 0x5efb; 0x048c; 0x005f; 0x9811; 0xd200;
    0x5fbd; 0x055b; 0x0060; 0x6204; 0x9000; 0x609e; 0x223e; 0x0061;
    0x080e; 0x1e00; 0x6241; 0x1265; 0x0063; 0x3a0b; 0x2400; 0x633d;
    0x0c7e; 0x0063; 0x570a; 0xef00; 0x6372; 0x066a; 0x0063; 0xc30a;
    0x2d00; 0x647a; 0x095c; 0x0064; 0xb009; 0xab00; 0x64e2; 0x0b37;
    0x0065; 0xa70c; 0xe800; 0x6666; 0x0496; 0x0066; 0xb52a; 0x1100;
    0x6753; 0x081f; 0x0067; 0x5608; 0xf100; 0x6897; 0x06e4; 0x0069;
    0x620b; 0xd800; 0x696f; 0x087d; 0x0069; 0x8a07; 0x6d00; 0x6994;
    0x0eed; 0x0069; 0xcc0a; 0xfa00; 0x6a0b; 0x0c9f; 0x006a; 0x3d0a;
    0x8200; 0x6b4e; 0x0a8b; 0x006c; 0x7205; 0x9200; 0x6deb; 0x03d6;
    0x006e; 0xa203; 0xc800; 0x6eba; 0x0b3e; 0x0070; 0x150c; 0xd600;
    0x701e; 0x0bc2; 0x0070; 0x260a; 0xc900; 0x7058; 0x0bd5; 0x0070;
    0x7805; 0x9400; 0x707c; 0x0820; 0x0071; 0x4e09; 0xb300; 0x7152;
    0x22f1; 0x0071; 0x7d09; 0xb400; 0x723a; 0x0e0e; 0x0072; 0x4c0c;
    0x2300; 0x7259; 0x047d; 0x0072; 0xe115; 0x1300; 0x7337; 0x0e32;
    0x0075; 0x1107; 0x2100; 0x7515; 0x155e; 0x0075; 0x2615; 0x6100;
    0x75bc; 0x1583; 0x0077; 0xa50d; 0x3300; 0x7941; 0x0623; 0x0079;
    0x4705; 0x7000; 0x79b0; 0x0bf5; 0x0079; 0xe40c; 0x3500; 0x7a17;
    0x0cab; 0x007a; 0x7f09; 0xb600; 0x7ac8; 0x1647; 0x007b; 0x0805;
    0x9800; 0x7b75; 0x166e; 0x007b; 0xad09; 0xb700; 0x7bb8; 0x0c4e;
    0x007b; 0xc70d; 0x3900; 0x7bdd; 0x1687; 0x007c; 0x3e0e; 0xda00;
    0x7c7e; 0x0e04; 0x007c; 0x8206; 0x1500; 0x7feb; 0x0537; 0x007f;
    0xf005; 0x1f00; 0x8171; 0x1788; 0x0081; 0x7f0a; 0x5200; 0x8258;
    0x17bc; 0x0082; 0x9217; 0xcb00; 0x82a6; 0x038c; 0x0083; 0x2803;
    0xcb00; 0x845b; 0x04df; 0x0084; 0xec0d; 0x6d00; 0x8511; 0x0d34;
    0x0085; 0x3d0d; 0x2900; 0x85a9; 0x078b; 0x0085; 0xaf08; 0x9200;
    0x85f7; 0x0893; 0x0086; 0x5418; 0x6700; 0x86f8; 0x0a73; 0x0087;
    0x0318; 0x8400; 0x8755; 0x0906; 0x0088; 0x0518; 0x9f00; 0x8956;
    0x043e; 0x008a; 0x0a09; 0x3200; 0x8a1d; 0x191c; 0x008a; 0x3b0a;
    0xc400; 0x8a6e; 0x09bf; 0x008a; 0xb90c; 0x9a00; 0x8afa; 0x068b;
    0x008b; 0x0e0b; 0xd400; 0x8b2c; 0x0cbd; 0x008b; 0x7f1f; 0x3c00;
    0x8c79; 0x0cc7; 0x008c; 0xed0b; 0x6200; 0x8fbb; 0x0b06; 0x008f;
    0xbf0a; 0x7d00; 0x8fc2; 0x03e2; 0x008f; 0xc40d; 0xbc00; 0x8fe6;
    0x0476; 0x0090; 0x1709; 0x3c00; 0x9019; 0x0c33; 0x0090; 0x2203;
    0x8300; 0x903c; 0x0cb7; 0x0090; 0x410b; 0xc900; 0x905c; 0x0a33;
    0x0090; 0x6109; 0xe400; 0x912d; 0x0b32; 0x0091; 0x4b08; 0x5600;
    0x91dc; 0x04ec; 0x0093; 0x0607; 0x9000; 0x9375; 0x067a; 0x0093;
    0x9a0a; 0xfc00; 0x9453; 0x0e1c; 0x0096; 0x9906; 0x5000; 0x9771;
    0x1b06; 0x0097; 0x8404; 0xe700; 0x9798; 0x08e2; 0x0097; 0xad0d;
    0x4200; 0x98f4; 0x0395; 0x0099; 0x050e; 0x0100; 0x990c; 0x03fa;
    0x0099; 0x1007; 0xa500; 0x9957; 0x05d0; 0x0099; 0xc10c; 0x4900;
    0x9a19; 0x1b66; 0x009a; 0x4a24; 0xaf00; 0x9bab; 0x0791; 0x009b;
    0xd607; 0x8e00; 0x9c2f; 0x03cd; 0x009c; 0x520d; 0xb400; 0x9d09;
    0x1bd8; 0x009d; 0x6007; 0x1c00; 0x0000; 0x9700; 0x5026; 0x0000;
    0x50c5; 0x0000; 0x5132; 0x0000; 0x514e; 0x0000; 0x51a4; 0x0000;
    0x53c9; 0x0000; 0x53db; 0x0000; 0x53df; 0x0000; 0x54ac; 0x0000;
    0x54e8; 0x0000; 0x55b0; 0x0000; 0x5632; 0x0000; 0x5642; 0x0000;
    0x564c; 0x0000; 0x56c0; 0x0000; 0x5835; 0x0000; 0x5906; 0x0000;
    0x5a29; 0x0000; 0x5c51; 0x0000; 0x5c60; 0x0000; 0x5df7; 0x0000;
    0x5e96; 0x0000; 0x5edf; 0x0000; 0x5efb; 0x0000; 0x5f98; 0x0000;
    0x5fbd; 0x0000; 0x609e; 0x0000; 0x6108; 0x0000; 0x6241; 0x0000;
    0x633a; 0x0000; 0x633d; 0x0000; 0x6357; 0x0000; 0x6372; 0x0000;
    0x63c3; 0x0000; 0x647a; 0x0000; 0x64b0; 0x0000; 0x64e2; 0x0000;
    0x65a7; 0x0000; 0x6666; 0x0000; 0x66b5; 0x0000; 0x6717; 0x0000;
    0x6753; 0x0000; 0x6756; 0x0000; 0x6897; 0x0000; 0x6962; 0x0000;
    0x696f; 0x0000; 0x698a; 0x0000; 0x6994; 0x0000; 0x69cc; 0x0000;
    0x6a0b; 0x0000; 0x6a3d; 0x0000; 0x6b4e; 0x0000; 0x6c72; 0x0000;
    0x6deb; 0x0000; 0x6ea2; 0x0000; 0x6eba; 0x0000; 0x7015; 0x0000;
    0x701e; 0x0000; 0x7026; 0x0000; 0x7058; 0x0000; 0x7078; 0x0000;
    0x707c; 0x0000; 0x714e; 0x0000; 0x7152; 0x0000; 0x723a; 0x0000;
    0x724c; 0x0000; 0x7259; 0x0000; 0x72e1; 0x0000; 0x7526; 0x0000;
    0x75bc; 0x0000; 0x77a5; 0x0000; 0x7941; 0x0000; 0x7947; 0x0000;
    0x79b0; 0x0000; 0x79e4; 0x0000; 0x7a17; 0x0000; 0x7a7f; 0x0000;
    0x7ac8; 0x0000; 0x7b75; 0x0000; 0x7bad; 0x0000; 0x7bb8; 0x0000;
    0x7bc7; 0x0000; 0x7bdd; 0x0000; 0x7c82; 0x0000; 0x7feb; 0x0000;
    0x7ff0; 0x0000; 0x8171; 0x0000; 0x817f; 0x0000; 0x8258; 0x0000;
    0x8292; 0x0000; 0x82a6; 0x0000; 0x8328; 0x0000; 0x845b; 0x0000;
    0x84ec; 0x0000; 0x8511; 0x0000; 0x853d; 0x0000; 0x85a9; 0x0000;
    0x85af; 0x0000; 0x85f7; 0x0000; 0x8654; 0x0000; 0x86f8; 0x0000;
    0x8703; 0x0000; 0x8755; 0x0000; 0x8805; 0x0000; 0x8956; 0x0000;
    0x8a0a; 0x0000; 0x8a3b; 0x0000; 0x8a6e; 0x0000; 0x8ab9; 0x0000;
    0x8afa; 0x0000; 0x8b0e; 0x0000; 0x8b2c; 0x0000; 0x8b7f; 0x0000;
    0x8c79; 0x0000; 0x8ced; 0x0000; 0x8fbb; 0x0000; 0x8fbf; 0x0000;
    0x8fc2; 0x0000; 0x8fc4; 0x0000; 0x8fe6; 0x0000; 0x9017; 0x0000;
    0x9019; 0x0000; 0x9022; 0x0000; 0x903c; 0x0000; 0x905c; 0x0000;
    0x9061; 0x0000; 0x912d; 0x0000; 0x914b; 0x0000; 0x91dc; 0x0000;
    0x9306; 0x0000; 0x9375; 0x0000; 0x939a; 0x0000; 0x9453; 0x0000;
    0x9699; 0x0000; 0x9771; 0x0000; 0x9784; 0x0000; 0x9798; 0x0000;
    0x97ad; 0x0000; 0x98f4; 0x0000; 0x9905; 0x0000; 0x9910; 0x0000;
    0x9957; 0x0000; 0x99c1; 0x0000; 0x9a19; 0x0000; 0x9a4a; 0x0000;
    0x9bab; 0x0000; 0x9bd6; 0x0000; 0x9c2f; 0x0000; 0x9c52; 0x0000;
    0x9d09; 0x0000; 0x9d60; 0x0000; 0x0000; 0x0c00; 0x5307; 0x1e66;
    0x0053; 0x7f05; 0xb800; 0x5bdb; 0x1e85; 0x0060; 0x622f; 0x7a00;
    0x61f2; 0x2120; 0x0064; 0x6f2f; 0x7500; 0x6adb; 0x0609; 0x006f;
    0x230e; 0xd800; 0x6ff9; 0x2fcc; 0x0071; 0x490e; 0xd900; 0x7337;
    0x2fc9; 0x0086; 0x121f; 0x2f00; 0x0000; 0x0a00; 0x537f; 0x0000;
    0x6062; 0x0000; 0x717d; 0x0000; 0x7337; 0x0000; 0x7515; 0x0000;
    0x7b08; 0x0000; 0x7c3e; 0x0000; 0x7c7e; 0x0000; 0x8a1d; 0x0000;
    0x9041; 0x0000; 0x0000; 0x0300; 0x55a9; 0x2fcb; 0x0067; 0x171e;
    0xb800; 0x7511; 0x2f89; 0x0000; 0x0002; 0x0075; 0x1100; 0x0099;
    0x0c00; 0x0000; 0x0001; 0x0053; 0xa92f; 0xc700; 0x0000; 0x0100;
    0x53a9;
  ] 0x00


let unmarshaled =
  let u = Uchar.of_int in
  let ( --> ) x y = (x, y) in
  DecodeCmap.[
    u 0xFE00 --> (
      [
        { start_unicode_value = u 0x51A4; additional_count = 0 };
        { start_unicode_value = u 0x5C60; additional_count = 0 };
        { start_unicode_value = u 0x6753; additional_count = 0 };
        { start_unicode_value = u 0x6ADB; additional_count = 0 };
        { start_unicode_value = u 0x6EBA; additional_count = 0 };
        { start_unicode_value = u 0x6F23; additional_count = 0 };
        { start_unicode_value = u 0x7149; additional_count = 0 };
      ],
      [
        (u 0x4FAE, 8491); (u 0x50E7, 8471); (u 0x514D, 8497); (u 0x51DE, 12190);
        (u 0x52C9, 8493); (u 0x52E4, 8451); (u 0x5351, 8487); (u 0x559D, 7353);
        (u 0x5606, 8477); (u 0x5668, 8446); (u 0x5840, 8492); (u 0x585A, 7417);
        (u 0x58A8, 8495); (u 0x5C64, 8472); (u 0x5C6E, 9851); (u 0x5ECA, 12186);
        (u 0x6094, 8439); (u 0x6168, 8441); (u 0x618E, 8474); (u 0x61F2, 12202);
        (u 0x654F, 8490); (u 0x65E2, 8447); (u 0x6674, 7856); (u 0x6691, 8465);
        (u 0x6717, 12188); (u 0x6885, 8484); (u 0x6B04, 8500); (u 0x6BBA, 8457);
        (u 0x6D77, 8440); (u 0x6E1A, 7387); (u 0x6F22, 8445); (u 0x716E, 8460);
        (u 0x722B, 9452); (u 0x732A, 7921); (u 0x7422, 7407); (u 0x76CA, 7943);
        (u 0x7891, 8488); (u 0x793C, 7951); (u 0x793E, 8461); (u 0x7948, 8448);

        (u 0x7949, 8458); (u 0x7950, 8499); (u 0x7956, 8470); (u 0x795D, 8464);
        (u 0x795E, 7952); (u 0x7965, 7953); (u 0x798D, 8438); (u 0x798E, 8481);
        (u 0x798F, 7955); (u 0x7A40, 8456); (u 0x7A81, 8482); (u 0x7BC0, 8469);
        (u 0x7CBE, 7962); (u 0x7E09, 11380); (u 0x7E41, 8485); (u 0x7F72, 8466);
        (u 0x7FBD, 7970); (u 0x8005, 8462); (u 0x81ED, 8463); (u 0x8279, 8571);
        (u 0x8457, 8478); (u 0x8612, 12203); (u 0x865C, 8502); (u 0x8910, 8444);
        (u 0x8996, 8459); (u 0x8AF8, 7993); (u 0x8B01, 8434); (u 0x8B39, 8452);
        (u 0x8CD3, 8489); (u 0x8D08, 8475); (u 0x8FB6, 9453); (u 0x9038, 8003);
        (u 0x90FD, 8006); (u 0x9686, 8501); (u 0x96E3, 8483); (u 0x9756, 7959);
        (u 0x97FF, 8450); (u 0x983B, 7449); (u 0x985E, 8504); (u 0x98EF, 8067);

        (u 0x98FC, 8068); (u 0x9928, 8070); (u 0x9DB4, 8083);
      ]);
    u 0xFE01 --> (
      [],
      [
        (u 0x7DF4, 8507); (u 0x8279, 8570); (u 0x9038, 8433);
      ]);
    u 0xE0100 --> (
      [
        { start_unicode_value = u 0x5307; additional_count = 0 };
        { start_unicode_value = u 0x55A9; additional_count = 0 };
        { start_unicode_value = u 0x5BDB; additional_count = 0 };
        { start_unicode_value = u 0x61F2; additional_count = 0 };
        { start_unicode_value = u 0x646F; additional_count = 0 };
        { start_unicode_value = u 0x6ADB; additional_count = 0 };
        { start_unicode_value = u 0x6F23; additional_count = 0 };
        { start_unicode_value = u 0x6FF9; additional_count = 0 };
        { start_unicode_value = u 0x7149; additional_count = 0 };
        { start_unicode_value = u 0x8612; additional_count = 0 };
      ],
      [
        (u 0x5026, 1629); (u 0x50C5, 1501); (u 0x5132, 3579); (u 0x514E, 2902);
        (u 0x51A4, 3994); (u 0x53A9, 1009); (u 0x53C9, 1851); (u 0x53DB, 3178);
        (u 0x53DF, 4098); (u 0x54AC, 4134); (u 0x54E8, 2211); (u 0x55B0, 1538);
        (u 0x5632, 4199); (u 0x5642, 1013); (u 0x564C, 2513); (u 0x56C0, 4220);
        (u 0x5835, 2904); (u 0x5906, 10483); (u 0x5A29, 3392); (u 0x5C51, 1547);
        (u 0x5C60, 2907); (u 0x5DF7, 1747); (u 0x5E96, 3423); (u 0x5EDF, 3272);
        (u 0x5EFB, 1164); (u 0x5F98, 4562); (u 0x5FBD, 1371); (u 0x6062, 1168);
        (u 0x609E, 8766); (u 0x6108, 3614); (u 0x6241, 4709); (u 0x633A, 2852);
        (u 0x633D, 3198); (u 0x6357, 2799); (u 0x6372, 1642); (u 0x63C3, 2605);
        (u 0x647A, 2396); (u 0x64B0, 2475); (u 0x64E2, 2871); (u 0x65A7, 3304);

        (u 0x6666, 1174); (u 0x66B5, 10769); (u 0x6753, 2079); (u 0x6756, 2289);
        (u 0x6897, 1764); (u 0x6962, 3032); (u 0x696F, 2173); (u 0x698A, 1901);
        (u 0x6994, 3821); (u 0x69CC, 2810); (u 0x6A0B, 3231); (u 0x6A3D, 2690);
        (u 0x6B4E, 2699); (u 0x6C72, 1426); (u 0x6DEB, 982); (u 0x6EA2, 968);
        (u 0x6EBA, 2878); (u 0x7015, 3286); (u 0x701E, 3010); (u 0x7026, 2761);
        (u 0x7058, 3029); (u 0x7078, 1428); (u 0x707C, 2080); (u 0x714E, 2483);
        (u 0x7152, 8945); (u 0x717D, 2484); (u 0x723A, 3598); (u 0x724C, 3107);
        (u 0x7259, 1149); (u 0x72E1, 5395); (u 0x7337, 3634); (u 0x7511, 1825);
        (u 0x7515, 5470); (u 0x7526, 5473); (u 0x75BC, 5507); (u 0x77A5, 3379);
        (u 0x7941, 1571); (u 0x7947, 1392); (u 0x79B0, 3061); (u 0x79E4, 3125);

        (u 0x7A17, 3243); (u 0x7A7F, 2486); (u 0x7AC8, 5703); (u 0x7B08, 1432);
        (u 0x7B75, 5742); (u 0x7BAD, 2487); (u 0x7BB8, 3150); (u 0x7BC7, 3385);
        (u 0x7BDD, 5767); (u 0x7C3E, 3802); (u 0x7C7E, 3588); (u 0x7C82, 1557);
        (u 0x7FEB, 1335); (u 0x7FF0, 1311); (u 0x8171, 6024); (u 0x817F, 2642);
        (u 0x8258, 6076); (u 0x8292, 6091); (u 0x82A6, 908); (u 0x8328, 971);
        (u 0x845B, 1247); (u 0x84EC, 3437); (u 0x8511, 3380); (u 0x853D, 3369);
        (u 0x85A9, 1931); (u 0x85AF, 2194); (u 0x85F7, 2195); (u 0x8654, 6247);
        (u 0x86F8, 2675); (u 0x8703, 6276); (u 0x8755, 2310); (u 0x8805, 6303);
        (u 0x8956, 1086); (u 0x8A0A, 2354); (u 0x8A1D, 6428); (u 0x8A3B, 2756);
        (u 0x8A6E, 2495); (u 0x8AB9, 3226); (u 0x8AFA, 1675); (u 0x8B0E, 3028);

        (u 0x8B2C, 3261); (u 0x8B7F, 7996); (u 0x8C79, 3271); (u 0x8CED, 2914);
        (u 0x8FBB, 2822); (u 0x8FBF, 2685); (u 0x8FC2, 994); (u 0x8FC4, 3516);
        (u 0x8FE6, 1142); (u 0x9017, 2364); (u 0x9019, 3123); (u 0x9022, 899);
        (u 0x903C, 3255); (u 0x9041, 3017); (u 0x905C, 2611); (u 0x9061, 2532);
        (u 0x912D, 2866); (u 0x914B, 2134); (u 0x91DC, 1260); (u 0x9306, 1936);
        (u 0x9375, 1658); (u 0x939A, 2812); (u 0x9453, 3612); (u 0x9699, 1616);
        (u 0x9771, 6918); (u 0x9784, 1255); (u 0x9798, 2274); (u 0x97AD, 3394);
        (u 0x98F4, 917); (u 0x9905, 3585); (u 0x990C, 1018); (u 0x9910, 1957);
        (u 0x9957, 1488); (u 0x99C1, 3145); (u 0x9A19, 7014); (u 0x9A4A, 9391);
        (u 0x9BAB, 1937); (u 0x9BD6, 1934); (u 0x9C2F, 973); (u 0x9C52, 3508);

        (u 0x9D09, 7128); (u 0x9D60, 1820);
      ]);
    u 0xE0101 --> (
      [
        { start_unicode_value = u 0x5026; additional_count = 0 };
        { start_unicode_value = u 0x50C5; additional_count = 0 };
        { start_unicode_value = u 0x5132; additional_count = 0 };
        { start_unicode_value = u 0x514E; additional_count = 0 };
        { start_unicode_value = u 0x51A4; additional_count = 0 };
        { start_unicode_value = u 0x53C9; additional_count = 0 };
        { start_unicode_value = u 0x53DB; additional_count = 0 };
        { start_unicode_value = u 0x53DF; additional_count = 0 };
        { start_unicode_value = u 0x54AC; additional_count = 0 };
        { start_unicode_value = u 0x54E8; additional_count = 0 };

        { start_unicode_value = u 0x55B0; additional_count = 0 };
        { start_unicode_value = u 0x5632; additional_count = 0 };
        { start_unicode_value = u 0x5642; additional_count = 0 };
        { start_unicode_value = u 0x564C; additional_count = 0 };
        { start_unicode_value = u 0x56C0; additional_count = 0 };
        { start_unicode_value = u 0x5835; additional_count = 0 };
        { start_unicode_value = u 0x5906; additional_count = 0 };
        { start_unicode_value = u 0x5A29; additional_count = 0 };
        { start_unicode_value = u 0x5C51; additional_count = 0 };
        { start_unicode_value = u 0x5C60; additional_count = 0 };

        { start_unicode_value = u 0x5DF7; additional_count = 0 };
        { start_unicode_value = u 0x5E96; additional_count = 0 };
        { start_unicode_value = u 0x5EDF; additional_count = 0 };
        { start_unicode_value = u 0x5EFB; additional_count = 0 };
        { start_unicode_value = u 0x5F98; additional_count = 0 };
        { start_unicode_value = u 0x5FBD; additional_count = 0 };
        { start_unicode_value = u 0x609E; additional_count = 0 };
        { start_unicode_value = u 0x6108; additional_count = 0 };
        { start_unicode_value = u 0x6241; additional_count = 0 };
        { start_unicode_value = u 0x633A; additional_count = 0 };

        { start_unicode_value = u 0x633D; additional_count = 0 };
        { start_unicode_value = u 0x6357; additional_count = 0 };
        { start_unicode_value = u 0x6372; additional_count = 0 };
        { start_unicode_value = u 0x63C3; additional_count = 0 };
        { start_unicode_value = u 0x647A; additional_count = 0 };
        { start_unicode_value = u 0x64B0; additional_count = 0 };
        { start_unicode_value = u 0x64E2; additional_count = 0 };
        { start_unicode_value = u 0x65A7; additional_count = 0 };
        { start_unicode_value = u 0x6666; additional_count = 0 };
        { start_unicode_value = u 0x66B5; additional_count = 0 };

        { start_unicode_value = u 0x6717; additional_count = 0 };
        { start_unicode_value = u 0x6753; additional_count = 0 };
        { start_unicode_value = u 0x6756; additional_count = 0 };
        { start_unicode_value = u 0x6897; additional_count = 0 };
        { start_unicode_value = u 0x6962; additional_count = 0 };
        { start_unicode_value = u 0x696F; additional_count = 0 };
        { start_unicode_value = u 0x698A; additional_count = 0 };
        { start_unicode_value = u 0x6994; additional_count = 0 };
        { start_unicode_value = u 0x69CC; additional_count = 0 };
        { start_unicode_value = u 0x6A0B; additional_count = 0 };

        { start_unicode_value = u 0x6A3D; additional_count = 0 };
        { start_unicode_value = u 0x6B4E; additional_count = 0 };
        { start_unicode_value = u 0x6C72; additional_count = 0 };
        { start_unicode_value = u 0x6DEB; additional_count = 0 };
        { start_unicode_value = u 0x6EA2; additional_count = 0 };
        { start_unicode_value = u 0x6EBA; additional_count = 0 };
        { start_unicode_value = u 0x7015; additional_count = 0 };
        { start_unicode_value = u 0x701E; additional_count = 0 };
        { start_unicode_value = u 0x7026; additional_count = 0 };
        { start_unicode_value = u 0x7058; additional_count = 0 };

        { start_unicode_value = u 0x7078; additional_count = 0 };
        { start_unicode_value = u 0x707C; additional_count = 0 };
        { start_unicode_value = u 0x714E; additional_count = 0 };
        { start_unicode_value = u 0x7152; additional_count = 0 };
        { start_unicode_value = u 0x723A; additional_count = 0 };
        { start_unicode_value = u 0x724C; additional_count = 0 };
        { start_unicode_value = u 0x7259; additional_count = 0 };
        { start_unicode_value = u 0x72E1; additional_count = 0 };
        { start_unicode_value = u 0x7526; additional_count = 0 };
        { start_unicode_value = u 0x75BC; additional_count = 0 };

        { start_unicode_value = u 0x77A5; additional_count = 0 };
        { start_unicode_value = u 0x7941; additional_count = 0 };
        { start_unicode_value = u 0x7947; additional_count = 0 };
        { start_unicode_value = u 0x79B0; additional_count = 0 };
        { start_unicode_value = u 0x79E4; additional_count = 0 };
        { start_unicode_value = u 0x7A17; additional_count = 0 };
        { start_unicode_value = u 0x7A7F; additional_count = 0 };
        { start_unicode_value = u 0x7AC8; additional_count = 0 };
        { start_unicode_value = u 0x7B75; additional_count = 0 };
        { start_unicode_value = u 0x7BAD; additional_count = 0 };

        { start_unicode_value = u 0x7BB8; additional_count = 0 };
        { start_unicode_value = u 0x7BC7; additional_count = 0 };
        { start_unicode_value = u 0x7BDD; additional_count = 0 };
        { start_unicode_value = u 0x7C82; additional_count = 0 };
        { start_unicode_value = u 0x7FEB; additional_count = 0 };
        { start_unicode_value = u 0x7FF0; additional_count = 0 };
        { start_unicode_value = u 0x8171; additional_count = 0 };
        { start_unicode_value = u 0x817F; additional_count = 0 };
        { start_unicode_value = u 0x8258; additional_count = 0 };
        { start_unicode_value = u 0x8292; additional_count = 0 };

        { start_unicode_value = u 0x82A6; additional_count = 0 };
        { start_unicode_value = u 0x8328; additional_count = 0 };
        { start_unicode_value = u 0x845B; additional_count = 0 };
        { start_unicode_value = u 0x84EC; additional_count = 0 };
        { start_unicode_value = u 0x8511; additional_count = 0 };
        { start_unicode_value = u 0x853D; additional_count = 0 };
        { start_unicode_value = u 0x85A9; additional_count = 0 };
        { start_unicode_value = u 0x85AF; additional_count = 0 };
        { start_unicode_value = u 0x85F7; additional_count = 0 };
        { start_unicode_value = u 0x8654; additional_count = 0 };

        { start_unicode_value = u 0x86F8; additional_count = 0 };
        { start_unicode_value = u 0x8703; additional_count = 0 };
        { start_unicode_value = u 0x8755; additional_count = 0 };
        { start_unicode_value = u 0x8805; additional_count = 0 };
        { start_unicode_value = u 0x8956; additional_count = 0 };
        { start_unicode_value = u 0x8A0A; additional_count = 0 };
        { start_unicode_value = u 0x8A3B; additional_count = 0 };
        { start_unicode_value = u 0x8A6E; additional_count = 0 };
        { start_unicode_value = u 0x8AB9; additional_count = 0 };
        { start_unicode_value = u 0x8AFA; additional_count = 0 };

        { start_unicode_value = u 0x8B0E; additional_count = 0 };
        { start_unicode_value = u 0x8B2C; additional_count = 0 };
        { start_unicode_value = u 0x8B7F; additional_count = 0 };
        { start_unicode_value = u 0x8C79; additional_count = 0 };
        { start_unicode_value = u 0x8CED; additional_count = 0 };
        { start_unicode_value = u 0x8FBB; additional_count = 0 };
        { start_unicode_value = u 0x8FBF; additional_count = 0 };
        { start_unicode_value = u 0x8FC2; additional_count = 0 };
        { start_unicode_value = u 0x8FC4; additional_count = 0 };
        { start_unicode_value = u 0x8FE6; additional_count = 0 };

        { start_unicode_value = u 0x9017; additional_count = 0 };
        { start_unicode_value = u 0x9019; additional_count = 0 };
        { start_unicode_value = u 0x9022; additional_count = 0 };
        { start_unicode_value = u 0x903C; additional_count = 0 };
        { start_unicode_value = u 0x905C; additional_count = 0 };
        { start_unicode_value = u 0x9061; additional_count = 0 };
        { start_unicode_value = u 0x912D; additional_count = 0 };
        { start_unicode_value = u 0x914B; additional_count = 0 };
        { start_unicode_value = u 0x91DC; additional_count = 0 };
        { start_unicode_value = u 0x9306; additional_count = 0 };

        { start_unicode_value = u 0x9375; additional_count = 0 };
        { start_unicode_value = u 0x939A; additional_count = 0 };
        { start_unicode_value = u 0x9453; additional_count = 0 };
        { start_unicode_value = u 0x9699; additional_count = 0 };
        { start_unicode_value = u 0x9771; additional_count = 0 };
        { start_unicode_value = u 0x9784; additional_count = 0 };
        { start_unicode_value = u 0x9798; additional_count = 0 };
        { start_unicode_value = u 0x97AD; additional_count = 0 };
        { start_unicode_value = u 0x98F4; additional_count = 0 };
        { start_unicode_value = u 0x9905; additional_count = 0 };

        { start_unicode_value = u 0x9910; additional_count = 0 };
        { start_unicode_value = u 0x9957; additional_count = 0 };
        { start_unicode_value = u 0x99C1; additional_count = 0 };
        { start_unicode_value = u 0x9A19; additional_count = 0 };
        { start_unicode_value = u 0x9A4A; additional_count = 0 };
        { start_unicode_value = u 0x9BAB; additional_count = 0 };
        { start_unicode_value = u 0x9BD6; additional_count = 0 };
        { start_unicode_value = u 0x9C2F; additional_count = 0 };
        { start_unicode_value = u 0x9C52; additional_count = 0 };
        { start_unicode_value = u 0x9D09; additional_count = 0 };

        { start_unicode_value = u 0x9D60; additional_count = 0 };
      ],
      [
        (u 0x5307, 7782); (u 0x537F, 1464); (u 0x5BDB, 7813); (u 0x6062, 12154);
        (u 0x61F2, 8480); (u 0x646F, 12149); (u 0x6ADB, 1545); (u 0x6F23, 3800);
        (u 0x6FF9, 12236); (u 0x7149, 3801); (u 0x7337, 12233); (u 0x8612, 7983);
      ]);
    u 0xE0102 --> (
      [
        { start_unicode_value = u 0x537F; additional_count = 0 };
        { start_unicode_value = u 0x6062; additional_count = 0 };
        { start_unicode_value = u 0x717D; additional_count = 0 };
        { start_unicode_value = u 0x7337; additional_count = 0 };
        { start_unicode_value = u 0x7515; additional_count = 0 };
        { start_unicode_value = u 0x7B08; additional_count = 0 };
        { start_unicode_value = u 0x7C3E; additional_count = 0 };
        { start_unicode_value = u 0x7C7E; additional_count = 0 };
        { start_unicode_value = u 0x8A1D; additional_count = 0 };
        { start_unicode_value = u 0x9041; additional_count = 0 };
      ],
      [
        (u 0x55A9, 12235); (u 0x6717, 7864); (u 0x7511, 12169);
      ]);
    u 0xE0103 --> (
      [
        { start_unicode_value = u 0x7511; additional_count = 0 };
        { start_unicode_value = u 0x990C; additional_count = 0 };
      ],
      [
        (u 0x53A9, 12231);
      ]);
    u 0xE0105 --> (
      [
        { start_unicode_value = u 0x53A9; additional_count = 0 };
      ],
      []);
  ]
