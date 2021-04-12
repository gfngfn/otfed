
open Basic
open EncodeOperation.Open


include EncodeTable

include EncodeBasic


let get_table_tag (table : table) =
  table.tag


let get_contents (table : table) : string =
  table.contents


let compare_table (table1 : table) (table2 : table) =
  Value.Tag.compare table1.tag table2.tag


type relative_offset = int

type table_directory_entry = {
  table_tag       : Value.Tag.t;
  relative_offset : relative_offset;
  table_length    : int;
  table_checksum  : wint;
}

type table_accumulator = relative_offset option * table_directory_entry Alist.t


let add_checksum (x : wint) (y : wint) : wint =
  let open WideInt in
    let q = (of_int 1) lsl 32 in
    (x +% y) mod q


let calculate_checksum (s : string) : wint =
  let open WideInt in
  let len = String.length s in
  let access i =
    if i < len then
      of_byte (String.get s i)
    else
      !% 0
  in
  let rec aux acc i =
    if i >= len then
      acc
    else
      let b0 = access i in
      let b1 = access (i + 1) in
      let b2 = access (i + 2) in
      let b3 = access (i + 3) in
      let ui = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
      let acc = add_checksum acc ui in
      aux acc (i + 4)
  in
  aux (of_int 0) 0


(* `e_single_table` is used as a folding function in `enc_tables`. *)
let e_single_table ((checksum_reloffset_opt, entries) : table_accumulator) (table : table) =
  let open EncodeOperation in
  pad_to_long_aligned    >>= fun () ->
  current                >>= fun reloffset ->
  e_bytes table.contents >>= fun () ->
  let checksum_reloffset_opt =
    if Value.Tag.equal Value.Tag.table_head table.tag then
      Some(reloffset + 8)
    else
      checksum_reloffset_opt
  in
  let table_checksum = calculate_checksum table.contents in
  let entry =
    {
      table_tag       = table.tag;
      relative_offset = reloffset;
      table_length    = String.length table.contents;
      table_checksum  = table_checksum;
    }
  in
  return (checksum_reloffset_opt, Alist.extend entries entry)


(* `enc_tables tables` writes the tables `tables` and returns the pair of
   - an offset to `CheckSumAdjustment` relative to the position immediately after the table directory, and
   - all the entries for the construction of the table directory. *)
let enc_tables (tables : table list) : (relative_offset * table_directory_entry list) encoder =
  let open EncodeOperation in
  foldM e_single_table tables (None, Alist.empty) >>= fun (checksum_reloffset_opt, entries) ->
  match checksum_reloffset_opt with
  | None                     -> assert false
  | Some(checksum_reloffset) -> return (checksum_reloffset, Alist.to_list entries)


let enc_table_directory_entry ~first_offset (all_table_checksum : wint) (entry : table_directory_entry) : wint encoder =
  let open EncodeOperation in
  e_tag entry.table_tag                                >>= fun () ->
  e_uint32 entry.table_checksum                        >>= fun () ->
  e_uint32 (!% (first_offset + entry.relative_offset)) >>= fun () ->
  e_uint32 (!% (entry.table_length))                   >>= fun () ->
  return @@ add_checksum all_table_checksum entry.table_checksum


let enc_table_directory_entries ~first_offset (entries : table_directory_entry list) : wint encoder =
  let open EncodeOperation in
  foldM (enc_table_directory_entry ~first_offset) entries (!% 0)


let cut_uint32_to_bytes (u : wint) : char * char * char * char =
  let open WideInt in
  let b0 = u lsr 24 in
  let r0 = u -% (b0 lsl 24) in
  let b1 = r0 lsr 16 in
  let r1 = r0 -% (b1 lsl 16) in
  let b2 = r1 lsr 8 in
  let b3 = r1 -% (b2 lsl 8) in
  (to_byte b0, to_byte b1, to_byte b2, to_byte b3)


let update_checksum_adjustment ~checksum_offset ~checksum_value (contents : string) =
  let checksum_adjustment =
    let temp = (!%% 0xB1B0AFBAL) -% checksum_value in
    if WideInt.is_neg temp then temp +% (!% (1 lsl 32)) else temp
  in
  try
    let bytes = Bytes.of_string contents in
    let (b0, b1, b2, b3) = cut_uint32_to_bytes checksum_adjustment in
    Bytes.set bytes checksum_offset       b0;
    Bytes.set bytes (checksum_offset + 1) b1;
    Bytes.set bytes (checksum_offset + 2) b2;
    Bytes.set bytes (checksum_offset + 3) b3;
    Bytes.to_string bytes
  with
  | _ -> assert false


(* Writes the 12-byte header of the entire TrueType-based OpenType font. *)
let enc_header (numTables : int) =
  let open EncodeOperation in
  let entrySelector = Stdlib.(truncate (log (float_of_int numTables) /. log 2.0)) in
  let searchRange = (1 lsl entrySelector) * 16 in
  let rangeShift = numTables * 16 - searchRange in
  e_uint32 (!% 0x00010000) >>= fun () ->
  e_uint16 numTables       >>= fun () ->
  e_uint16 searchRange     >>= fun () ->
  e_uint16 entrySelector   >>= fun () ->
  e_uint16 rangeShift      >>= fun () ->
  return ()


let make_font_data_from_tables (tables : table list) : string ok =
  let tables = tables |> List.sort compare_table in
  let numTables = List.length tables in
  let first_offset = 12 + numTables * 16 in
    (* `first_offset` is the offset where the table directory ends. *)
  let open ResultMonad in
  enc_tables tables |> EncodeOperation.run >>= fun (table_contents, (checksum_reloffset, entries)) ->
  let enc =
    let open EncodeOperation in
    enc_header numTables                              >>= fun () ->
    enc_table_directory_entries ~first_offset entries >>= fun all_table_checksum ->
    e_bytes table_contents                            >>= fun () ->
    return all_table_checksum
  in
  enc |> EncodeOperation.run >>= fun (contents, all_table_checksum) ->
  let checksum_offset = first_offset + checksum_reloffset in
  let prelude_checksum = calculate_checksum (String.sub contents 0 first_offset) in
  let checksum_value = add_checksum prelude_checksum all_table_checksum in
  return (update_checksum_adjustment ~checksum_offset ~checksum_value contents)



module ForTest = struct
  module EncodeOperation = EncodeOperation

  type 'a encoder = 'a EncodeOperation.Open.encoder

  let run enc =
    let open ResultMonad in
    enc |> EncodeOperation.run >>= fun (contents, _) ->
    return contents
end
