
type 'a set = 'a list

module ImmutStack : sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val size : 'a t -> int
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a t * 'a) option
  val pop_all : 'a t -> 'a list
end = struct
  type 'a t = 'a list

  let empty = []

  let is_empty = function
    | []     -> true
    | _ :: _ -> false

  let size = List.length

  let push x stack = x :: stack

  let pop = function
    | []      -> None
    | x :: xs -> Some((xs, x))

  let pop_all stack = stack
end

module Alist : sig
  type 'a t
  val empty : 'a t
  val extend : 'a t -> 'a -> 'a t
  val append : 'a t -> 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val is_empty : 'a t -> bool
  val chop_last : 'a t -> ('a t * 'a) option
end = struct
  type 'a t = 'a list

  let empty = []

  let extend acc x = x :: acc

  let append acc xs = List.rev_append xs acc

  let to_list = List.rev

  let is_empty = function
    | []     -> true
    | _ :: _ -> false

  let chop_last acc =
    match acc with
    | []      -> None
    | x :: xs -> Some((xs, x))
end

module ResultMonad = struct
  let ( >>= ) x f =
    match x with
    | Ok(v)           -> f v
    | Error(_) as err -> err

  let return v =
    Ok(v)

  let err e =
    Error(e)

  let mapM f xs =
    let res =
      xs |> List.fold_left (fun res x ->
        res >>= fun acc ->
        f x >>= fun y ->
        return @@ Alist.extend acc y
      ) (Ok(Alist.empty))
    in
    res >>= fun acc ->
    return @@ Alist.to_list acc

  let foldM f xs acc =
    xs |> Base.List.fold_result ~f ~init:acc
(*
    xs |> List.fold_left (fun res x ->
      res >>= fun acc ->
      f acc x
    ) (Ok(acc))
*)
end

type offset = int
[@@deriving show { with_path = false }]

module WideInt = struct
  type t = Int64.t

  let equal = Int64.equal

  let compare = Int64.compare

  let pp fmt iw = Format.fprintf fmt "%LX" iw

  let ( lsl ) = Int64.shift_left

  let ( lsr ) = Int64.shift_right

  let ( lor ) = Int64.logor

  let ( land ) = Int64.logand

  let ( mod ) = Int64.rem

  let add = Int64.add

  let sub = Int64.sub

  let div = Int64.div

  let of_int = Int64.of_int

  let to_int = Int64.to_int

  let of_int64 iw = iw

  let to_int64 iw = iw

  let of_byte ch = Int64.of_int (Char.code ch)

  let to_byte iw = Char.chr (Int64.to_int iw)  (* -- may raise 'Invalid_argument' -- *)

  let is_in_uint32 iw = Int64.zero <= iw && iw < 0x100000000L

  let is_in_int32 iw = -0x80000000L <= iw && iw < 0x80000000L

  let is_in_int64 iw = Int64.min_int <= iw && iw <= Int64.max_int

  let is_neg iw = iw < Int64.zero

  let is_pos iw = iw > Int64.zero
end

type wint = WideInt.t
[@@deriving show {with_path = false}]

let ( +% ) = WideInt.add

let ( -% ) = WideInt.sub

let ( /% ) = WideInt.div

let ( !% ) = WideInt.of_int

let ( !%% ) = WideInt.of_int64

let is_in_range ~lower:a ~upper:b x = (a <= x && x <= b)

let pp_uchar ppf uchar =
  Format.fprintf ppf "U+%04X" (Uchar.to_int uchar)

let pp_option pp ppf = function
  | None   -> Format.fprintf ppf "None"
  | Some v -> Format.fprintf ppf "Some@[<v2>@,%a@]" pp v


(* `https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6post.html` *)
let macintosh_glyph_name_list =
  [
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
