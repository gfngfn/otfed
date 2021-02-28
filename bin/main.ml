
let print_table_directory (common, _) =
  let tables = Otfed.Decode.tables common in
  Printf.printf "tables:\n";
  tables |> List.iter (fun tag ->
    Printf.printf "- %s\n" (Otfed.Value.Tag.to_string tag)
  )


let print_cmap (common, _) =
  let open Otfed.Decode in
  Printf.printf "cmap:\n";
  let _ =
    let open Otfed.ResultMonad in
    cmap common >>= fun icmap ->
    Intermediate.Cmap.get_subtables icmap >>= fun subtables ->
    subtables |> List.iter (fun subtable ->
      let ids = Intermediate.Cmap.get_subtable_ids subtable in
      Printf.printf "- subtable (platform: %d, encoding: %d, format: %d)\n"
        ids.platform_id
        ids.encoding_id
        ids.format;
      Intermediate.Cmap.fold_subtable subtable (fun () seg ->
        match seg with
        | Incremental(uch1, uch2, gid) ->
            if Uchar.equal uch1 uch2 then
              Printf.printf "  - I U+%04X --> %d\n" (Uchar.to_int uch1) gid
            else
              Printf.printf "  - I U+%04X, U+%04X --> %d\n" (Uchar.to_int uch1) (Uchar.to_int uch2) gid

        | Constant(uch1, uch2, gid) ->
            Printf.printf "  - C U+%04X, U+%04X --> %d\n" (Uchar.to_int uch1) (Uchar.to_int uch2) gid
      ) () |> ignore;
    );
    return ()
  in
  ()


let print_head (common, _) =
  let open Otfed.Decode in
  let _ =
    let open Otfed.ResultMonad in
    Printf.printf "head:\n";
    head common >>= fun head ->
    Format.printf "%a\n" Otfed.Value.Head.pp head;
    return ()
  in
  ()


let print_hhea (common, _) =
  let open Otfed.Decode in
  let _ =
    let open Otfed.ResultMonad in
    Printf.printf "hhea:\n";
    hhea common >>= fun hhea ->
    Format.printf "%a\n" Otfed.Value.Hhea.pp hhea;
    return ()
  in
  ()


let _ =
  let s = Core_kernel.In_channel.read_all Sys.argv.(1) in
  let open Otfed.ResultMonad in
  Otfed.Decode.source_of_string s >>= function
  | Single(source) ->
      print_table_directory source;
      print_head source;
      print_hhea source;
      print_cmap source;
      return ()

  | Collection(sources) ->
      sources |> List.iter print_table_directory;
      return ()
