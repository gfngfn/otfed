
module Alist = Otfed.Alist
module ResultMonad = Otfed.ResultMonad


type config = {
  cmap : bool;
  head : bool;
  hhea : bool;
  maxp : bool;
  glyf : Otfed.Value.glyph_id Alist.t;
}

type error =
  | UnknownCommand     of string
  | InvalidCommandLine
  | CannotReadFile     of string
  | DecodingError      of Otfed.Decode.Error.t
[@@deriving show { with_path = false }]

let inj = function
  | Ok(v)    -> Ok(v)
  | Error(e) -> Error(DecodingError(e))


let print_table_directory (common, _) =
  let tables = Otfed.Decode.tables common in
  Printf.printf "tables:\n";
  tables |> List.iter (fun tag ->
    Printf.printf "- %s\n" (Otfed.Value.Tag.to_string tag)
  )


let print_cmap (common, _) =
  let open Otfed.Decode in
  Printf.printf "cmap:\n";
  let res =
    let open ResultMonad in
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
  res |> inj


let print_head (common, _) =
  let open Otfed.Decode in
  let res =
    let open ResultMonad in
    Printf.printf "head:\n";
    head common >>= fun head ->
    Format.printf "%a\n" Otfed.Value.Head.pp head;
    return ()
  in
  res |> inj


let print_hhea (common, _) =
  let open Otfed.Decode in
  let res =
    let open ResultMonad in
    Printf.printf "hhea:\n";
    hhea common >>= fun hhea ->
    Format.printf "%a\n" Otfed.Value.Hhea.pp hhea;
    return ()
  in
  res |> inj


let print_maxp (common, _) =
  let open Otfed.Decode in
  let res =
    let open ResultMonad in
    Printf.printf "maxp:\n";
    maxp common >>= fun maxp ->
    Format.printf "%a\n" Otfed.Value.Maxp.pp maxp;
    return ()
  in
  res |> inj


let parse_args () =
  let open ResultMonad in
  let rec aux n acc i =
    if i >= n then
      return acc
    else
      match Sys.argv.(i) with
      | "cmap" -> aux n { acc with cmap = true } (i + 1)
      | "head" -> aux n { acc with head = true } (i + 1)
      | "hhea" -> aux n { acc with hhea = true } (i + 1)
      | "maxp" -> aux n { acc with maxp = true } (i + 1)

      | "glyf" ->
          let gid = int_of_string (Sys.argv.(i + 1)) in
          aux n { acc with glyf = Alist.extend acc.glyf gid } (i + 2)

      | s ->
          err @@ UnknownCommand(s)
  in
  try
    let n = Array.length Sys.argv in
    let path = Sys.argv.(1) in
    let config =
      {
        cmap = false;
        head = false;
        hhea = false;
        maxp = false;
        glyf = Alist.empty;
      }
    in
    aux n config 2 >>= fun config ->
    return (config, path)
  with
  | _ ->
      err InvalidCommandLine


let read_file path =
  let open ResultMonad in
  try
    let s = Core_kernel.In_channel.read_all path in
    return s
  with
  | _ ->
      err @@ CannotReadFile(path)


let _ =
  let res =
    let open ResultMonad in
    parse_args () >>= fun (config, path) ->
    read_file path >>= fun s ->
    Otfed.Decode.source_of_string s |> inj >>= function
    | Single(source) ->
        print_table_directory source;
        begin
          if config.head then print_head source else return ()
        end >>= fun () ->
        begin
          if config.hhea then print_hhea source else return ()
        end >>= fun () ->
        begin
          if config.maxp then print_maxp source else return ()
        end >>= fun () ->
        begin
          if config.cmap then print_cmap source else return ()
        end >>= fun () ->
        return ()

    | Collection(sources) ->
        sources |> List.iter print_table_directory;
        return ()
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> Format.printf "%a\n" pp_error e
