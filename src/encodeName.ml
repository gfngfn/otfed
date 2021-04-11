
open Basic
open EncodeBasic
open EncodeOperation.Open


type relative_offset = int

type name_record_and_offset = Value.Name.name_record * relative_offset


(* Makes (possibly a portion of) string storage from NameRecords.
   The offsets in return values are relative to the beggining of the string storage.  *)
let make_name_string_storage (name_records : Value.Name.name_record list) : (string * name_record_and_offset list) ok =
  let enc =
    let open EncodeOperation in
    let open Value.Name in
    name_records |> mapM (fun name_record ->
      current                  >>= fun reloffset ->
      e_bytes name_record.name >>= fun () ->
      return (name_record, reloffset)
    )
  in
  enc |> EncodeOperation.run


(* Makes a portion of string storage from LangTagRecords.
   The offsets in return values are relative to
   the end of the district of the string storage composed by NameRecords.  *)
let make_lang_tag_string_storage (lang_tags : Value.Name.lang_tag list) : (string * (string * relative_offset) list) ok =
  let enc =
    let open EncodeOperation in
    lang_tags |> mapM (fun lang_tag ->
      current          >>= fun reloffset ->
      e_bytes lang_tag >>= fun () ->
      return (lang_tag, reloffset)
    )
  in
  enc |> EncodeOperation.run


let e_name_records (names_and_reloffsets : name_record_and_offset list) : unit encoder =
  let open EncodeOperation in
  let open Value.Name in
  names_and_reloffsets |> e_list (fun (r, reloffset) ->
    (* Here, `reloffset` is relative to the beginning of the string storage.  *)
    let length = String.length r.name in
    e_uint16 r.platform_id >>= fun () ->
    e_uint16 r.encoding_id >>= fun () ->
    e_uint16 r.language_id >>= fun () ->
    e_uint16 r.name_id     >>= fun () ->
    e_uint16 length        >>= fun () ->
    e_uint16 reloffset
  )


let e_lang_tag_records ~starts_at:(starts_at : relative_offset) (lang_tags_and_reloffsets : (string * relative_offset) list) =
  let open EncodeOperation in
  lang_tags_and_reloffsets |> e_list (fun (lang_tag, reloffset) ->
    (* Here, `reloffset` is relative to the end of the area of the string storage where names are stored.  *)
    let length = String.length lang_tag in
    e_uint16 length >>= fun () ->
    e_uint16 (starts_at + reloffset)
  )


let make (name : Value.Name.t) =
  let open ResultMonad in
  let name_records = name.name_records in
  let count = List.length name_records in
  make_name_string_storage name_records >>= fun (storage1, names_and_reloffsets) ->
  let length_storage1 = String.length storage1 in
  match name.lang_tags with
  | None ->
      let offset_string_storage = 6 + 12 * count in
      let enc =
        let open EncodeOperation in
        e_uint16 0                          >>= fun () -> (* format number *)
        e_uint16 count                      >>= fun () ->
        e_uint16 offset_string_storage      >>= fun () ->
        e_name_records names_and_reloffsets >>= fun () ->
        e_bytes storage1
      in
      enc |> EncodeOperation.run

  | Some(lang_tags) ->
      let lang_tag_count = List.length lang_tags in
      let offset_string_storage = 6 + 12 * count + 4 * lang_tag_count in
      make_lang_tag_string_storage lang_tags >>= fun (storage2, lang_tags_and_reloffsets) ->
      let enc =
        let open EncodeOperation in
        e_uint16 1                          >>= fun () -> (* format number *)
        e_uint16 count                      >>= fun () ->
        e_uint16 offset_string_storage      >>= fun () ->
        e_name_records names_and_reloffsets >>= fun () ->
        e_uint16 lang_tag_count             >>= fun () ->
        e_lang_tag_records
          ~starts_at:length_storage1
          lang_tags_and_reloffsets          >>= fun () ->
        e_bytes storage1                    >>= fun () ->
        e_bytes storage2
      in
      enc |> EncodeOperation.run
