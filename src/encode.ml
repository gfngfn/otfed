
open Basic


module Error = EncodeError
module Subset = EncodeSubset


type 'a ok = ('a, Error.t) result

type table = string (* TODO *)


module Head = struct

  let make (loc_format : Intermediate.loc_format) (ihead : Intermediate.Head.t) : table ok =
    let d = ihead.Intermediate.Head.derived in
    let v = ihead.Intermediate.Head.value in
    let loc_format_num =
      let open Intermediate in
      match loc_format with
      | ShortLocFormat -> 0
      | LongLocFormat  -> 1
    in
    let magic_number = !% 0x5F0F3CF5 in
    let font_direction_hint = 0 in
    let glyph_data_format = 0 in
    let enc =
      let open EncodeOperation in
      e_uint32 (!% 0x00010000)    >>= fun () -> (* Table Version Number *)
      e_uint32 v.font_revision    >>= fun () ->
      e_uint32 (!% 0)             >>= fun () -> (* `checkSumAdjustment`: will be updated afterwards *)
      e_uint32 magic_number       >>= fun () ->
      e_uint16 v.flags            >>= fun () ->
      e_uint16 v.units_per_em     >>= fun () ->
      e_timestamp v.created       >>= fun () ->
      e_timestamp v.modified      >>= fun () ->
      e_int16 d.xmin              >>= fun () ->
      e_int16 d.ymin              >>= fun () ->
      e_int16 d.xmax              >>= fun () ->
      e_int16 d.ymax              >>= fun () ->
      e_uint16 v.mac_style        >>= fun () ->
      e_uint16 v.lowest_rec_ppem  >>= fun () ->
      e_int16 font_direction_hint >>= fun () ->
      e_int16 loc_format_num      >>= fun () ->
      e_int16 glyph_data_format   >>= fun () ->
      return ()
    in
    let open ResultMonad in
    enc |> EncodeOperation.run >>= fun (s, ()) ->
    return s

end
