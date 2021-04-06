
open Basic
open Value
open DecodeBasic
open DecodeOperation.Open


let d_mac_style : Head.mac_style decoder =
  let open DecodeOperation in
  d_uint16 >>= fun u ->
  if u land (65535 - 127) = 0 then
    return Value.Head.{
      bold      = (u land 1 > 0);
      italic    = (u land 2 > 0);
      underline = (u land 4 > 0);
      outline   = (u land 8 > 0);
      shadow    = (u land 16 > 0);
      condensed = (u land 32 > 0);
      extended  = (u land 64 > 0);
    }
  else
    err @@ Error.UnexpectedMacStyle(u)


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
      d_int16      >>= fun x_min ->
      d_int16      >>= fun y_min ->
      d_int16      >>= fun x_max ->
      d_int16      >>= fun y_max ->
      d_mac_style  >>= fun mac_style ->
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
          x_min;
          y_min;
          x_max;
          y_max;
        };
      }
  in
  DecodeOperation.run common.core offset dec
