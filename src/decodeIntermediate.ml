
open Basic
open DecodeBasic


module GeneralTable = struct

  type t = {
    core   : common_source_core;
    offset : offset;
    length : int;
  }


  let make (core : common_source_core) (offset : offset) (length : int) : t =
    {
      core   = core;
      offset = offset;
      length = length;
    }

end


module Cmap = struct

  include GeneralTable


  type subtable = t * offset * Value.Cmap.subtable_ids


  open DecodeOperation


  let d_fetch_long (origin : offset) d =
    current >>= fun pos_before ->
    d_uint32_int >>= fun reloffset ->
    let offset = origin + reloffset in
    seek offset >>= fun () ->
    d >>= fun v ->
    seek (pos_before + 4) >>= fun () ->
    return (offset, v)


  let d_encoding_record (cmap : t) : subtable decoder =
    d_uint16 >>= fun platform_id ->
    d_uint16 >>= fun encoding_id ->
    d_fetch_long cmap.offset d_uint16 >>= fun (offset, format) ->
    let ids =
      Value.Cmap.{
        platform_id = platform_id;
        encoding_id = encoding_id;
        format      = format;
      }
    in
    return @@ (cmap, offset, ids)


  let get_subtables (cmap : t) : (subtable set) ok =
    let open DecodeOperation in
    let dec =
      d_uint16 >>= fun version ->
      if version <> 0 then
        err @@ UnknownTableVersion(!% version)
      else
        d_list (d_encoding_record cmap) >>= fun raw_subtables ->
        let subtables =
          raw_subtables |> List.filter (fun (_, _, ids) ->
            let open Value.Cmap in
            match ids.format with
            | 4 | 12 | 13 ->
                begin
                  match (ids.platform_id, ids.encoding_id) with
                  | (0, _)  (* Unicode *)
                  | (3, 1)  (* Windows, UCS-2 *)
                  | (3, 10) (* Windows, UCS-4 *)
                  | (1, _)  (* Macintosh *)
                      -> true

                  | _ -> false
                end

            | _ ->
                false
          )
        in
        return subtables
    in
    run cmap.core cmap.offset dec

end
