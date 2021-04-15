
open Basic
open EncodeBasic
open EncodeOperation.Open
open Intermediate.Cff


module Maxp = EncodeCffMaxp


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


let e_dict (_dict : dict) =
  failwith "TODO: e_dict"


let e_lexical_charstring (_tokens : lexical_charstring) =
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


let make_dict_from_top_dict (_top_dict : top_dict) : dict =
  failwith "TODO: make_dict_from_top_dict"


let e_cff_first (name : string) (top_dict : top_dict) (string_index : string list) (global_subrs : lexical_charstring list) =
  let open EncodeOperation in
  let dict = make_dict_from_top_dict top_dict in
  e_cff_header >>= fun () ->
  e_index_singleton e_bytes name >>= fun () ->
  e_index_singleton e_dict dict >>= fun () ->
  e_index e_bytes string_index >>= fun () ->
  e_index e_lexical_charstring global_subrs
