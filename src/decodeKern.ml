
open Basic
open Value
open DecodeBasic


include GeneralTable(struct type t = unit end)


let get (src : source) : (t option) ok =
  let open ResultMonad in
  let common = get_common_source src in
  match DecodeOperation.seek_table common.table_directory Tag.table_kern with
  | None ->
      return None

  | Some((offset, length)) ->
      return @@ Some(make_scheme common.core offset length ())


type kern_info = {
  horizontal   : bool;
  minimum      : bool;
  cross_stream : bool;
}
[@@deriving show {with_path = false}]


let d_kern_info =
  let open DecodeOperation in
  d_uint16 >>= fun coverage ->
  let format = coverage lsr 8 in
  let kern_info =
    {
      horizontal   = (coverage land 1 > 0);
      minimum      = (coverage land 2 > 0);
      cross_stream = (coverage land 4 > 0);
    }
  in
  return (format, kern_info)


let rec d_kerning_pairs j p acc =
  let open DecodeOperation in
  if j <= 0 then
    return acc
  else
    d_uint16 >>= fun left ->
    d_uint16 >>= fun right ->
    d_int16 >>= fun values ->
    let acc = p acc left right values in
    d_kerning_pairs (j - 1) p acc


let rec d_kerning_tables i t p acc =
  let open DecodeOperation in
  if i <= 0 then
    return acc
  else
    d_uint16 >>= fun version ->
    match version with
    | 0 ->
        d_uint16 >>= fun length ->
        d_kern_info >>= fun (format, kern_info) ->
        begin
          match format with
          | 2 ->
              d_skip (length - 6) >>= fun () ->
              d_kerning_tables (i - 1) t p acc

          | 0 ->
              let (do_fold, acc) = t acc kern_info in
              if do_fold then
                d_uint16 >>= fun nPairs ->
                d_kerning_pairs nPairs p acc >>= fun acc ->
                d_kerning_tables (i - 1) t p acc
              else
                d_skip (length - 6) >>= fun () ->
                d_kerning_tables (i - 1) t p acc

          | _ ->
              err @@ UnknownFormatNumber(format)
        end

    | _ ->
        err @@ UnknownTableVersion(!% version)


let fold t p acc ikern =
  let dec =
    let open DecodeOperation in
    (* Only the Windows version of `kern` Table is supported;
       the Apple version, which has a 32-bit version number, *)
    d_uint16 >>= fun version ->
    match version with
    | 0 ->
        d_uint16 >>= fun nTables ->
        d_kerning_tables nTables t p acc

    | _ ->
        err @@ UnknownTableVersion(!% version)
  in
  dec |> DecodeOperation.run ikern.core ikern.offset
