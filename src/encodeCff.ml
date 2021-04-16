
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
    next_sid = 390;
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
  e_uint16 1                    >>= fun () ->
  e_offsize offsize             >>= fun () ->
  e_reloffset offsize (len + 1) >>= fun () ->
  e_bytes contents


let e_index (enc : 'a -> unit encoder) (xs : 'a list) : unit encoder =
  let open EncodeOperation in
  match xs with
  | [] ->
      e_uint16 0

  | _ :: _ ->
      let enc =
        foldM (fun (len_acc, pos_prev, count) x ->
          enc x >>= fun () ->
          current >>= fun pos ->
          let len = pos - pos_prev in
          return @@ (Alist.extend len_acc len, pos, count + 1)
        ) xs (Alist.empty, 0, 0) >>= fun (len_acc, pos_last, count) ->
        transform_result @@ make_offsize (pos_last + 1) >>= fun offsize ->
        return @@ (Alist.to_list len_acc, offsize, count)
      in
      transform_result (enc |> EncodeOperation.run) >>= fun (contents, (lens, offsize, count)) ->
      let reloffsets = 1 :: lens |> List.map (fun len -> len + 1) in
      e_uint16 count                          >>= fun () ->
      e_offsize offsize                       >>= fun () ->
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


let e_real (r : float) =
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
  let open EncodeOperation in
  match value with
  | Integer(n) ->
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

  | Real(r) ->
      e_real r


let e_dict_entry ((key, values) : key * value list) : unit encoder =
  let open EncodeOperation in
  e_list e_value values >>= fun () ->
  match key with
  | ShortKey(n) ->
      e_uint8 n

  | LongKey(n) ->
      e_uint8 12 >>= fun () ->
      e_uint8 n


let e_dict (dict : dict) =
  let open EncodeOperation in
  let entries = DictMap.bindings dict in
  e_list e_dict_entry entries


let e_subroutine (_subr : lexical_charstring) =
  failwith "TODO: e_lexical_charstring"


let e_cff_header : unit encoder =
  let open EncodeOperation in
  let (major, minor) = (1, 0) in
  let hdrsize = 4 in
  let offsize = OffSize3 in
  e_uint8   major   >>= fun () ->
  e_uint8   minor   >>= fun () ->
  e_uint8   hdrsize >>= fun () ->
  e_offsize offsize


let make_dict_from_top_dict (string_index : StringIndex.t) (top_dict : top_dict) : (StringIndex.t * dict) ok =
  let open ResultMonad in
  let zero_offset_CharString_INDEX = failwith "TODO" in
  let
    {
      font_name = _;
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
      font_bbox = (bbox_elem1, bbox_elem2, bbox_elem3, bbox_elem4);
      stroke_width;
      cid_info = _;
      number_of_glyphs = _;
    } = top_dict
  in
  let (string_index, sid_version)     = string_index |> StringIndex.add version in
  let (string_index, sid_notice)      = string_index |> StringIndex.add notice in
  let (string_index, sid_copyright)   = string_index |> StringIndex.add copyright in
  let (string_index, sid_full_name)   = string_index |> StringIndex.add full_name in
  let (string_index, sid_family_name) = string_index |> StringIndex.add family_name in
  let (string_index, sid_weight)      = string_index |> StringIndex.add weight in
  let charstring_type = 2 in
  let dict =
    List.fold_left (fun dict (key, values) ->
      dict |> DictMap.add key values
    ) DictMap.empty [
      (ShortKey(0),  [Integer(sid_version)]);
      (ShortKey(1),  [Integer(sid_notice)]);
      (LongKey(0),   [Integer(sid_copyright)]);
      (ShortKey(2),  [Integer(sid_full_name)]);
      (ShortKey(3),  [Integer(sid_family_name)]);
      (ShortKey(4),  [Integer(sid_weight)]);
      (LongKey(1),   [Integer(if is_fixed_pitch then 1 else 0)]);
      (LongKey(2),   [Integer(italic_angle)]);
      (LongKey(3),   [Integer(underline_position)]);
      (LongKey(4),   [Integer(underline_thickness)]);
      (LongKey(5),   [Integer(paint_type)]);
      (LongKey(6),   [Integer(charstring_type)]);
      (ShortKey(5),  [Integer(bbox_elem1); Integer(bbox_elem2); Integer(bbox_elem3); Integer(bbox_elem4)]);
      (LongKey(8),   [Integer(stroke_width)]);
      (ShortKey(17), [Integer(zero_offset_CharString_INDEX)]);
    ]
  in
  return (string_index, dict)


let e_cff_first (name : string) (top_dict : top_dict) (string_index : StringIndex.t) (gsubrs : lexical_charstring list) =
  let open EncodeOperation in
  transform_result @@ make_dict_from_top_dict string_index top_dict >>= fun (string_index, dict) ->
  e_cff_header                                                      >>= fun () ->
  e_index_singleton e_bytes name                                    >>= fun () ->
  e_index_singleton e_dict dict                                     >>= fun () ->
  e_index e_bytes (StringIndex.to_list string_index)                >>= fun () ->
  e_index e_subroutine gsubrs
