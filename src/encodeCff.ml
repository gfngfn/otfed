
open Basic
open EncodeBasic
open EncodeOperation.Open
open Intermediate.Cff


module Maxp = EncodeCffMaxp


module StringIndex : sig
  type sid = int
  type t
  val empty : t
  val add : string -> t -> t * sid
  val to_list : t -> string list
end = struct

  type sid = int

  type t = {
    next_sid : sid;
    strings  : string Alist.t;
  }


  let empty = {
    next_sid = 391;
    strings  = Alist.empty;
  }


  let add (s : string) (index : t) : t * sid =
    let sid = index.next_sid in
    let index =
      {
        next_sid = sid + 1;
        strings  = Alist.extend index.strings s;
      }
    in
    (index, sid)


  let to_list (index : t) : string list =
    index.strings |> Alist.to_list

end


let e_offsize (offsize : offsize) : unit encoder =
  let open EncodeOperation in
  let u =
    match offsize with
    | OffSize1 -> 1
    | OffSize2 -> 2
    | OffSize3 -> 3
    | OffSize4 -> 4
  in
  e_uint8 u


let e_reloffset (offsize : offsize) (reloffset : int) =
  let open EncodeOperation in
  match offsize with
  | OffSize1 -> e_uint8 reloffset
  | OffSize2 -> e_uint16 reloffset
  | OffSize3 -> e_uint24 reloffset
  | OffSize4 -> e_uint32 (!% reloffset)


let make_offsize (len : int) : offsize ok =
  let open ResultMonad in
  if len < (1 lsl 8) then
    return OffSize1
  else if len < (1 lsl 16) then
    return OffSize2
  else if len < (1 lsl 24) then
    return OffSize3
  else if !% len < WideInt.(of_int 1 lsl 32) then
    return OffSize4
  else
    err @@ Error.TooLargeToDetermineOffSize(len)


let e_index_singleton (enc : 'a -> unit encoder) (x : 'a) : unit encoder =
  let open EncodeOperation in
  transform_result (enc x |> EncodeOperation.run) >>= fun (contents, ()) ->
  let len = String.length contents in
  transform_result (make_offsize len) >>= fun offsize ->
  e_uint16 1                    >>= fun () -> (* The number of entries *)
  e_offsize offsize             >>= fun () ->
  e_reloffset offsize 1         >>= fun () -> (* The first offset *)
  e_reloffset offsize (len + 1) >>= fun () ->
  e_bytes contents


let e_index (enc : 'a -> unit encoder) (xs : 'a list) : unit encoder =
  let open EncodeOperation in
  match xs with
  | [] ->
      e_uint16 0

  | _ :: _ ->
      let enc =
        foldM (fun (pos_acc, _pos_prev, count) x ->
          enc x >>= fun () ->
          current >>= fun pos ->
          return @@ (Alist.extend pos_acc pos, pos, count + 1)
        ) xs (Alist.empty, 0, 0) >>= fun (pos_acc, pos_last, count) ->
        transform_result @@ make_offsize (pos_last + 1) >>= fun offsize ->
        return @@ (Alist.to_list pos_acc, offsize, count)
      in
      transform_result (enc |> EncodeOperation.run) >>= fun (contents, (poss, offsize, count)) ->
      let reloffsets = poss |> List.map (fun pos -> pos + 1) in
      e_uint16 count                          >>= fun () ->
      e_offsize offsize                       >>= fun () ->
      e_reloffset offsize 1                   >>= fun () -> (* The first offset *)
      e_list (e_reloffset offsize) reloffsets >>= fun () ->
      e_bytes contents


let e_twoscompl2 (n : int) =
  let open EncodeOperation in
  e_int16 n


let e_twoscompl4 (n : int) =
  let open EncodeOperation in
  e_int32 (!% n)


let quad_dot     = 0xa
let quad_e_plus  = 0xb
let quad_e_minus = 0xc
let quad_minus   = 0xe


let e_integer_value (n : int) : unit encoder =
  let open EncodeOperation in
  if -107 <= n && n <= 107 then
    e_uint8 (n + 139)
  else if 108 <= n && n <= 1131 then
    let u = n - 108 in
    let c0 = u lsr 8 in
    let b0 = c0 + 247 in
    let b1 = u - (c0 lsl 8) in
    e_uint8 b0 >>= fun () ->
    e_uint8 b1
  else if -1131 <= n && n <= 108 then
    let u = - (n + 108) in
    let c0 = u lsr 8 in
    let b0 = c0 + 251 in
    let b1 = u - (c0 lsl 8) in
    e_uint8 b0 >>= fun () ->
    e_uint8 b1
  else if -32768 <= n && n <= 32767 then
    e_uint8 28 >>= fun () ->
    e_twoscompl2 n
  else if - (1 lsl 31) <= n && n <= (1 lsl 31) - 1 then
    e_uint8 29 >>= fun () ->
    e_twoscompl4 n
  else
    err @@ Error.NotEncodableAsDictValue(n)


let e_real_value (r : float) : unit encoder =
  let rec aux (acc : int Alist.t) = function
    | [] ->
        Alist.to_list acc

    | 'e' :: '+' :: chs ->
        aux (Alist.extend acc quad_e_plus) chs

    | 'e' :: '-' :: chs ->
        aux (Alist.extend acc quad_e_minus) chs

    | '-' :: chs ->
        aux (Alist.extend acc quad_minus) chs

    | '.' :: chs ->
        aux (Alist.extend acc quad_dot) chs

    | ch :: chs ->
        if '0' <= ch && ch <= '9' then
          let q = (Char.code ch - Char.code '0') in
          aux (Alist.extend acc q) chs
        else
          assert false
  in
  let open EncodeOperation in
  let rec e_quads = function
    | [] ->
        e_uint8 0xff

    | [q] ->
        e_uint8 ((q lsl 4) lor 0xf)

    | q1 :: q2 :: qs ->
        e_uint8 ((q1 lsl 4) lor q2) >>= fun () ->
        e_quads qs
  in
  let s = Printf.sprintf "%e" r in
  let chs = Core_kernel.String.to_list s in
  let qs = aux Alist.empty chs in
  e_uint8 30 >>= fun () ->
  e_quads qs


let e_value (value : value) : unit encoder =
  match value with
  | Integer(n) -> e_integer_value n
  | Real(r)    -> e_real_value r


let e_short_key (n : int) : unit encoder =
  let open EncodeOperation in
  e_uint8 n


let e_long_key (n : int) : unit encoder =
  let open EncodeOperation in
  e_uint8 12 >>= fun () ->
  e_uint8 n


let e_dict_entry ((key, values) : key * value list) : unit encoder =
  let open EncodeOperation in
  e_list e_value values >>= fun () ->
  match key with
  | ShortKey(n) -> e_short_key n
  | LongKey(n)  -> e_long_key n


let e_dict (dict : dict) =
  let open EncodeOperation in
  let entries = DictMap.bindings dict in
  e_list e_dict_entry entries


let e_charstring_token (ctoken : charstring_token) : unit encoder =
  let open EncodeOperation in
  match ctoken with
  | ArgumentInteger(n) -> e_integer_value n
  | ArgumentReal(r)    -> e_real_value r

  | OpHStem   -> e_short_key 1
  | OpVStem   -> e_short_key 3
  | OpHStemHM -> e_short_key 18
  | OpVStemHM -> e_short_key 23

  | OpRMoveTo -> e_short_key 21
  | OpHMoveTo -> e_short_key 22
  | OpVMoveTo -> e_short_key 4

  | OpRLineTo -> e_short_key 5
  | OpHLineTo -> e_short_key 6
  | OpVLineTo -> e_short_key 7

  | OpCallSubr  -> err @@ Error.Unsupported(Error.LocalSubrOperation)
  | OpCallGSubr -> e_short_key 29

  | OpReturn  -> e_short_key 11
  | OpEndChar -> e_short_key 14

  | OpHintMask(stem_arg) ->
      e_short_key 19 >>= fun () ->
      e_bytes stem_arg

  | OpCntrMask(stem_arg) ->
      e_short_key 20 >>= fun () ->
      e_bytes stem_arg

  | OpRCurveLine -> e_short_key 24
  | OpRLineCurve -> e_short_key 25
  | OpRRCurveTo  -> e_short_key 8
  | OpVVCurveTo  -> e_short_key 26
  | OpHHCurveTo  -> e_short_key 27
  | OpVHCurveTo  -> e_short_key 30
  | OpHVCurveTo  -> e_short_key 31

  | OpHFlex  -> e_long_key 34
  | OpFlex   -> e_long_key 35
  | OpHFlex1 -> e_long_key 36
  | OpFlex1  -> e_long_key 37


let e_charstring (charstring : lexical_charstring) : unit encoder =
  let open EncodeOperation in
  e_list e_charstring_token charstring


let e_cff_header : unit encoder =
  let open EncodeOperation in
  let (major, minor) = (1, 0) in
  let hdrsize = 4 in
  let offsize = OffSize3 in
  e_uint8   major   >>= fun () ->
  e_uint8   minor   >>= fun () ->
  e_uint8   hdrsize >>= fun () ->
  e_offsize offsize


let add_string_if_exists (s_opt : string option) (string_index : StringIndex.t) =
  match s_opt with
  | None ->
      (string_index, None)

  | Some(s) ->
      let (string_index, sid) = string_index |> StringIndex.add s in
      (string_index, Some(sid))


let e_charset ~(sid_first_opt : int option) ~(num_glyphs : int) =
  let open EncodeOperation in
  match sid_first_opt with
  | None ->
      return ()

  | Some(sid_first) ->
      e_uint8 2           >>= fun () -> (* Format number *)
      e_uint16 sid_first  >>= fun () ->
      e_uint16 (num_glyphs - 2) (* `.notdef` and the first glyph for the SID is excluded *)


let e_cff (top_dict : top_dict) ~(gsubrs : lexical_charstring list) ~(names_and_charstrings : (string * lexical_charstring) list) =
  let open EncodeOperation in
  let string_index = StringIndex.empty in
  let (string_index, sid_first_opt, lcs_acc, num_glyphs) =
    names_and_charstrings |> List.fold_left (fun (string_index, sid_first_opt, lcs_acc, i) ((name, lcs) : string * lexical_charstring) ->
      let (string_index, sid_first_opt) =
        if i = 0 then
          (string_index, None)
        else if i = 1 then
          let (string_index, sid) = string_index |> StringIndex.add name in
          (string_index, Some(sid))
        else
          let (string_index, _) = string_index |> StringIndex.add name in
          (string_index, sid_first_opt)
      in
      (string_index, sid_first_opt, Alist.extend lcs_acc lcs, i + 1)
    ) (string_index, None, Alist.empty, 0)
  in
  let charstrings = Alist.to_list lcs_acc in
  let
    {
      font_name = name;
      version;
      notice;
      copyright;
      full_name;
      family_name;
      weight;
      is_fixed_pitch;
      italic_angle;
      underline_position;
      underline_thickness;
      paint_type;
      font_bbox;
      stroke_width;
      cid_info = _;
      number_of_glyphs = _;
    } = top_dict
  in
  let (string_index, sid_version_opt)     = string_index |> add_string_if_exists version in
  let (string_index, sid_notice_opt)      = string_index |> add_string_if_exists notice in
  let (string_index, sid_copyright_opt)   = string_index |> add_string_if_exists copyright in
  let (string_index, sid_full_name_opt)   = string_index |> add_string_if_exists full_name in
  let (string_index, sid_family_name_opt) = string_index |> add_string_if_exists family_name in
  let (string_index, sid_weight_opt)      = string_index |> add_string_if_exists weight in
  let strings = StringIndex.to_list string_index in
  transform_result @@ run e_cff_header                       >>= fun (contents_header, ()) ->
  transform_result @@ run (e_index_singleton e_bytes name)   >>= fun (contents_name_index, ()) ->
  transform_result @@ run (e_index e_bytes strings)          >>= fun (contents_string_index, ()) ->
  transform_result @@ run (e_index e_charstring charstrings) >>= fun (contents_charstring_index, ()) ->
  transform_result @@ run (e_index e_charstring gsubrs)      >>= fun (contents_gsubrs_index, ()) ->
  transform_result @@ run (e_dict DictMap.empty)             >>= fun (contents_private, ()) ->
  transform_result @@ run (e_charset ~sid_first_opt ~num_glyphs) >>= fun (contents_charset, ()) ->
  let length_upper_bound_of_top_dict_index =
    List.fold_left ( + ) 0 [
      2;      (* count *)
      1;      (* offsize *)
      8;      (* a couple of offsets *)
      2 * 16; (* keys *)
      5 * 20; (* values *)
    ]
  in
  let zero_offset_CharString_INDEX =
    List.fold_left ( + ) 0 [
      String.length contents_header;
      String.length contents_name_index;
      length_upper_bound_of_top_dict_index;
      String.length contents_string_index;
      String.length contents_gsubrs_index;
    ]
  in
  let zero_offset_private = zero_offset_CharString_INDEX + String.length contents_charstring_index in
  let zero_offset_charset = zero_offset_private + String.length contents_private in
  let optional_entries =
    List.fold_left (fun entries (key, sid_opt) ->
      match sid_opt with
      | None      -> entries
      | Some(sid) -> (key, [Integer(sid)]) :: entries
    ) [] [
      (ShortKey(0),  sid_version_opt);
      (ShortKey(1),  sid_notice_opt);
      (LongKey(0),   sid_copyright_opt);
      (ShortKey(2),  sid_full_name_opt);
      (ShortKey(3),  sid_family_name_opt);
      (ShortKey(4),  sid_weight_opt);
    ]
  in
  let dict =
    let Value.{ x_min; y_min; x_max; y_max } = font_bbox in
    let charstring_type = 2 in
    List.fold_left (fun dict (key, values) ->
      dict |> DictMap.add key values
    ) DictMap.empty (List.append optional_entries [
      (LongKey(1),   [Integer(if is_fixed_pitch then 1 else 0)]);
      (LongKey(2),   [Integer(italic_angle)]);
      (LongKey(3),   [Integer(underline_position)]);
      (LongKey(4),   [Integer(underline_thickness)]);
      (LongKey(5),   [Integer(paint_type)]);
      (LongKey(6),   [Integer(charstring_type)]);
      (ShortKey(5),  [Integer(x_min); Integer(y_min); Integer(x_max); Integer(y_max)]);
      (LongKey(8),   [Integer(stroke_width)]);
      (ShortKey(15), [Integer(zero_offset_charset)]);
      (ShortKey(17), [Integer(zero_offset_CharString_INDEX)]);
      (ShortKey(18), [Integer(String.length contents_private); Integer(zero_offset_private)]);
    ])
  in
  transform_result @@ run (e_index_singleton e_dict dict) >>= fun (contents_top_dict_index, ()) ->
  let length_for_padding = length_upper_bound_of_top_dict_index - String.length contents_top_dict_index in
  e_bytes contents_header           >>= fun () ->
  e_bytes contents_name_index       >>= fun () ->
  e_bytes contents_top_dict_index   >>= fun () ->
  e_bytes contents_string_index     >>= fun () ->
  e_bytes contents_gsubrs_index     >>= fun () ->
  e_paddings length_for_padding     >>= fun () ->
  e_bytes contents_charstring_index >>= fun () ->
  e_bytes contents_private          >>= fun () ->
  e_bytes contents_charset


let make (top_dict : top_dict) ~(gsubrs : lexical_charstring list) ~(names_and_charstrings : (string * lexical_charstring) list) =
  let open ResultMonad in
  e_cff top_dict ~gsubrs ~names_and_charstrings |> EncodeOperation.run >>= fun (contents, ()) ->
  return {
    tag = Value.Tag.table_cff;
    contents;
  }
