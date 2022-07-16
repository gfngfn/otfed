
open Basic
open DecodeBasic
open DecodeOperation.Open


(* `https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6post.html` *)
let macintosh_glyph_name =
  Array.of_list [
  (*   0 *) ".notdef";
  (*   1 *) ".null";
  (*   2 *) "nonmarkingreturn";
  (*   3 *) "space";
  (*   4 *) "exclam";
  (*   5 *) "quotedbl";
  (*   6 *) "numbersign";
  (*   7 *) "dollar";
  (*   8 *) "percent";
  (*   9 *) "ampersand";
  (*  10 *) "quotesingle";
  (*  11 *) "parenleft";
  (*  12 *) "parenright";
  (*  13 *) "asterisk";
  (*  14 *) "plus";
  (*  15 *) "comma";
  (*  16 *) "hyphen";
  (*  17 *) "period";
  (*  18 *) "slash";
  (*  19 *) "zero";
  (*  20 *) "one";
  (*  21 *) "two";
  (*  22 *) "three";
  (*  23 *) "four";
  (*  24 *) "five";
  (*  25 *) "six";
  (*  26 *) "seven";
  (*  27 *) "eight";
  (*  28 *) "nine";
  (*  29 *) "colon";
  (*  30 *) "semicolon";
  (*  31 *) "less";
  (*  32 *) "equal";
  (*  33 *) "greater";
  (*  34 *) "question";
  (*  35 *) "at";
  (*  36 *) "A";
  (*  37 *) "B";
  (*  38 *) "C";
  (*  39 *) "D";
  (*  40 *) "E";
  (*  41 *) "F";
  (*  42 *) "G";
  (*  43 *) "H";
  (*  44 *) "I";
  (*  45 *) "J";
  (*  46 *) "K";
  (*  47 *) "L";
  (*  48 *) "M";
  (*  49 *) "N";
  (*  50 *) "O";
  (*  51 *) "P";
  (*  52 *) "Q";
  (*  53 *) "R";
  (*  54 *) "S";
  (*  55 *) "T";
  (*  56 *) "U";
  (*  57 *) "V";
  (*  58 *) "W";
  (*  59 *) "X";
  (*  60 *) "Y";
  (*  61 *) "Z";
  (*  62 *) "bracketleft";
  (*  63 *) "backslash";
  (*  64 *) "bracketright";
  (*  65 *) "asciicircum";
  (*  66 *) "underscore";
  (*  67 *) "grave";
  (*  68 *) "a";
  (*  69 *) "b";
  (*  70 *) "c";
  (*  71 *) "d";
  (*  72 *) "e";
  (*  73 *) "f";
  (*  74 *) "g";
  (*  75 *) "h";
  (*  76 *) "i";
  (*  77 *) "j";
  (*  78 *) "k";
  (*  79 *) "l";
  (*  80 *) "m";
  (*  81 *) "n";
  (*  82 *) "o";
  (*  83 *) "p";
  (*  84 *) "q";
  (*  85 *) "r";
  (*  86 *) "s";
  (*  87 *) "t";
  (*  88 *) "u";
  (*  89 *) "v";
  (*  90 *) "w";
  (*  91 *) "x";
  (*  92 *) "y";
  (*  93 *) "z";
  (*  94 *) "braceleft";
  (*  95 *) "bar";
  (*  96 *) "braceright";
  (*  97 *) "asciitilde";
  (*  98 *) "Adieresis";
  (*  99 *) "Aring";
  (* 100 *) "Ccedilla";
  (* 101 *) "Eacute";
  (* 102 *) "Ntilde";
  (* 103 *) "Odieresis";
  (* 104 *) "Udieresis";
  (* 105 *) "aacute";
  (* 106 *) "agrave";
  (* 107 *) "acircumflex";
  (* 108 *) "adieresis";
  (* 109 *) "atilde";
  (* 110 *) "aring";
  (* 111 *) "ccedilla";
  (* 112 *) "eacute";
  (* 113 *) "egrave";
  (* 114 *) "ecircumflex";
  (* 115 *) "edieresis";
  (* 116 *) "iacute";
  (* 117 *) "igrave";
  (* 118 *) "icircumflex";
  (* 119 *) "idieresis";
  (* 120 *) "ntilde";
  (* 121 *) "oacute";
  (* 122 *) "ograve";
  (* 123 *) "ocircumflex";
  (* 124 *) "odieresis";
  (* 125 *) "otilde";
  (* 126 *) "uacute";
  (* 127 *) "ugrave";
  (* 128 *) "ucircumflex";
  (* 129 *) "udieresis";
  (* 130 *) "dagger";
  (* 131 *) "degree";
  (* 132 *) "cent";
  (* 133 *) "sterling";
  (* 134 *) "section";
  (* 135 *) "bullet";
  (* 136 *) "paragraph";
  (* 137 *) "germandbls";
  (* 138 *) "registered";
  (* 139 *) "copyright";
  (* 140 *) "trademark";
  (* 141 *) "acute";
  (* 142 *) "dieresis";
  (* 143 *) "notequal";
  (* 144 *) "AE";
  (* 145 *) "Oslash";
  (* 146 *) "infinity";
  (* 147 *) "plusminus";
  (* 148 *) "lessequal";
  (* 149 *) "greaterequal";
  (* 150 *) "yen";
  (* 151 *) "mu";
  (* 152 *) "partialdiff";
  (* 153 *) "summation";
  (* 154 *) "product";
  (* 155 *) "pi";
  (* 156 *) "integral";
  (* 157 *) "ordfeminine";
  (* 158 *) "ordmasculine";
  (* 159 *) "Omega";
  (* 160 *) "ae";
  (* 161 *) "oslash";
  (* 162 *) "questiondown";
  (* 163 *) "exclamdown";
  (* 164 *) "logicalnot";
  (* 165 *) "radical";
  (* 166 *) "florin";
  (* 167 *) "approxequal";
  (* 168 *) "Delta";
  (* 169 *) "guillemotleft";
  (* 170 *) "guillemotright";
  (* 171 *) "ellipsis";
  (* 172 *) "nonbreakingspace";
  (* 173 *) "Agrave";
  (* 174 *) "Atilde";
  (* 175 *) "Otilde";
  (* 176 *) "OE";
  (* 177 *) "oe";
  (* 178 *) "endash";
  (* 179 *) "emdash";
  (* 180 *) "quotedblleft";
  (* 181 *) "quotedblright";
  (* 182 *) "quoteleft";
  (* 183 *) "quoteright";
  (* 184 *) "divide";
  (* 185 *) "lozenge";
  (* 186 *) "ydieresis";
  (* 187 *) "Ydieresis";
  (* 188 *) "fraction";
  (* 189 *) "currency";
  (* 190 *) "guilsinglleft";
  (* 191 *) "guilsinglright";
  (* 192 *) "fi";
  (* 193 *) "fl";
  (* 194 *) "daggerdbl";
  (* 195 *) "periodcentered";
  (* 196 *) "quotesinglbase";
  (* 197 *) "quotedblbase";
  (* 198 *) "perthousand";
  (* 199 *) "Acircumflex";
  (* 200 *) "Ecircumflex";
  (* 201 *) "Aacute";
  (* 202 *) "Edieresis";
  (* 203 *) "Egrave";
  (* 204 *) "Iacute";
  (* 205 *) "Icircumflex";
  (* 206 *) "Idieresis";
  (* 207 *) "Igrave";
  (* 208 *) "Oacute";
  (* 209 *) "Ocircumflex";
  (* 210 *) "apple";
  (* 211 *) "Ograve";
  (* 212 *) "Uacute";
  (* 213 *) "Ucircumflex";
  (* 214 *) "Ugrave";
  (* 215 *) "dotlessi";
  (* 216 *) "circumflex";
  (* 217 *) "tilde";
  (* 218 *) "macron";
  (* 219 *) "breve";
  (* 220 *) "dotaccent";
  (* 221 *) "ring";
  (* 222 *) "cedilla";
  (* 223 *) "hungarumlaut";
  (* 224 *) "ogonek";
  (* 225 *) "caron";
  (* 226 *) "Lslash";
  (* 227 *) "lslash";
  (* 228 *) "Scaron";
  (* 229 *) "scaron";
  (* 230 *) "Zcaron";
  (* 231 *) "zcaron";
  (* 232 *) "brokenbar";
  (* 233 *) "Eth";
  (* 234 *) "eth";
  (* 235 *) "Yacute";
  (* 236 *) "yacute";
  (* 237 *) "Thorn";
  (* 238 *) "thorn";
  (* 239 *) "minus";
  (* 240 *) "multiply";
  (* 241 *) "onesuperior";
  (* 242 *) "twosuperior";
  (* 243 *) "threesuperior";
  (* 244 *) "onehalf";
  (* 245 *) "onequarter";
  (* 246 *) "threequarters";
  (* 247 *) "franc";
  (* 248 *) "Gbreve";
  (* 249 *) "gbreve";
  (* 250 *) "Idotaccent";
  (* 251 *) "Scedilla";
  (* 252 *) "scedilla";
  (* 253 *) "Cacute";
  (* 254 *) "cacute";
  (* 255 *) "Ccaron";
  (* 256 *) "ccaron";
  (* 257 *) "dcroat";
  ]


let d_pascal_string_names (len_remained : int) : (string array) decoder =
  let open DecodeOperation in
  let rec aux acc len_remained =
    if len_remained <= 0 then
      return @@ Array.of_list (Alist.to_list acc)
    else
      d_uint8 >>= fun len ->
      d_bytes len >>= fun name ->
      aux (Alist.extend acc name) (len_remained - 1 - len)
  in
  aux Alist.empty len_remained


let lookup_name (names : string array) (index : int) =
  let open DecodeOperation in
  try
    let name =
      if index < 258 then
        macintosh_glyph_name.(index)
      else
        names.(index - 258)
    in
    return name
  with
  | _ -> err @@ Error.InvalidGlyphNameIndex(index)



let d_version2 ~(num_glyphs : int) ~(remained_length : int) : Value.Post.GlyphNameArray.t decoder =
  let open DecodeOperation in
  d_skip 2 >>= fun () -> (* numberOfGlyphs *)
  d_repeat num_glyphs d_uint16 >>= fun indices ->
  d_pascal_string_names (remained_length - 2 - 2 * num_glyphs) >>= fun names ->
  indices |> mapM (lookup_name names) >>= fun gid_indexed_names ->
  return @@ Value.Post.GlyphNameArray.of_list gid_indexed_names


let d_post ~(num_glyphs : int) ~(length : int) =
  let open DecodeOperation in
  d_uint32 >>= fun version ->
  begin
    if version = !% 0x00010000 then
      return false
    else if version = !% 0x00020000 then
      return true
    else if version = !% 0x00030000 then
      return false
    else
      err @@ Error.UnknownTableVersion(version)
  end >>= fun is_long ->
  d_uint32 >>= fun italic_angle ->
  d_int16  >>= fun underline_position ->
  d_int16  >>= fun underline_thickness ->
  d_uint32 >>= fun is_fixed_pitch ->
  d_uint32 >>= fun min_mem_type_42 ->
  d_uint32 >>= fun max_mem_type_42 ->
  d_uint32 >>= fun min_mem_type_1 ->
  d_uint32 >>= fun max_mem_type_1 ->
  begin
    if is_long then
      d_version2 ~num_glyphs ~remained_length:(length - 32) >>= fun names ->
      return @@ Some(names)
    else
      return None
  end >>= fun names_opt ->
  return Value.Post.{
    value = {
      italic_angle = WideInt.to_int italic_angle;
      underline_position;
      underline_thickness;
      is_fixed_pitch  = (WideInt.is_pos is_fixed_pitch);
      min_mem_type_42 = WideInt.to_int min_mem_type_42;
      max_mem_type_42 = WideInt.to_int max_mem_type_42;
      min_mem_type_1  = WideInt.to_int min_mem_type_1;
      max_mem_type_1  = WideInt.to_int max_mem_type_1;
    };
    glyph_names = names_opt;
  }


let get (src : source) : Value.Post.t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_post >>= fun (offset, length) ->
  let num_glyphs = common.num_glyphs in
  d_post ~num_glyphs ~length |> DecodeOperation.run common.core offset
