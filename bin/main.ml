
let print (common, _) =
  let open Otfed.ResultMonad in
  let _ =
    let tables = Otfed.Decode.tables common in
    tables |> List.iter (fun tag -> print_endline (Otfed.Value.Tag.to_string tag));
    return ()
  in
  ()


let print_cmap (common, _) =
  let open Otfed.Decode in
  let open Otfed.ResultMonad in
  let _ =
    cmap common >>= fun icmap ->
    Intermediate.Cmap.get_subtables icmap >>= fun subtables ->
    subtables |> List.iter (fun subtable ->
      let ids = Intermediate.Cmap.get_subtable_ids subtable in
      Printf.printf "platform: %d, encoding: %d, format: %d\n"
        ids.platform_id
        ids.encoding_id
        ids.format;
      Intermediate.Cmap.fold_subtable subtable (fun () seg ->
        match seg with
        | Incremental(uch1, uch2, gid) ->
            if Uchar.equal uch1 uch2 then
              Printf.printf "  I U+%04X --> %d\n" (Uchar.to_int uch1) gid
            else
              Printf.printf "  I U+%04X, U+%04X --> %d\n" (Uchar.to_int uch1) (Uchar.to_int uch2) gid

        | Constant(uch1, uch2, gid) ->
            Printf.printf "  C U+%04X, U+%04X --> %d\n" (Uchar.to_int uch1) (Uchar.to_int uch2) gid
      ) () |> ignore;
    );
    return ()
  in
  ()


let _ =
  let s = Core_kernel.In_channel.read_all Sys.argv.(1) in
  let open Otfed.ResultMonad in
  Otfed.Decode.source_of_string s >>= function
  | Single(source) ->
      print source;
      print_cmap source;
      return ()

  | Collection(sources) ->
      sources |> List.iter print;
      return ()
