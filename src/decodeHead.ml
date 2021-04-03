
open Basic
open DecodeBasic


let get (src : source) : Intermediate.Head.t ok =
  let open ResultMonad in
  let common = get_common_source src in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_head >>= fun (offset, _length) ->
  let dec =
    let open DecodeOperation in
    d_uint32 >>= fun version ->
    if version <> !%% 0x00010000L then
      err @@ UnknownTableVersion(version)
    else
      d_uint32     >>= fun font_revision ->
      d_skip 8     >>= fun () -> (* Skips `checkSumAdjustement` and `magicNumber`. *)
      d_uint16     >>= fun flags ->
      d_uint16     >>= fun units_per_em ->
      d_timestamp  >>= fun created ->
      d_timestamp  >>= fun modified ->
      d_int16      >>= fun xmin ->
      d_int16      >>= fun ymin ->
      d_int16      >>= fun xmax ->
      d_int16      >>= fun ymax ->
      d_uint16     >>= fun mac_style ->
      d_uint16     >>= fun lowest_rec_ppem ->
      (* Skips `fontDirectionHint` and `indexToLocFormat`. *)
      return Intermediate.Head.{
        value = Value.Head.{
          font_revision;
          flags;
          units_per_em;
          created;
          modified;
          mac_style;
          lowest_rec_ppem;
        };
        derived = {
          xmin;
          ymin;
          xmax;
          ymax;
        };
      }
  in
  DecodeOperation.run common.core offset dec
