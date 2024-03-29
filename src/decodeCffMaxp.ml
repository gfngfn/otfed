
open Basic
open DecodeBasic


let d_maxp =
  let open DecodeOperation in
  d_uint32 >>= fun version ->
  if version = !%% 0x00005000L then
    d_uint16 >>= fun num_glyphs ->
    return Intermediate.Cff.Maxp.{
      num_glyphs = num_glyphs;
    }
  else if version = !%% 0x00010000L then
    err @@ Error.CffContainsTtfMaxpTable
  else
    err @@ Error.UnknownTableVersion(version)


let get (cff : cff_source) : Intermediate.Cff.Maxp.t ok =
  let open ResultMonad in
  let common = cff.cff_common in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_maxp >>= fun (offset, _length) ->
  d_maxp |> DecodeOperation.run common.core offset
