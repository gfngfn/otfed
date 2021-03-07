
module Alist = Otfed.Alist
module ResultMonad = Otfed.ResultMonad
module D = Otfed.Decode
module V = Otfed.Value


type config = {
  cmap : bool;
  head : bool;
  hhea : bool;
  maxp : bool;
  glyf : (V.glyph_id * string) Alist.t;
  cff  : V.glyph_id Alist.t;
}

type error =
  | UnknownCommand     of string
  | InvalidCommandLine
  | CannotReadFile     of string
  | CannotWriteFile    of string
  | DecodingError      of D.Error.t
[@@deriving show { with_path = false }]

let inj = function
  | Ok(v)    -> Ok(v)
  | Error(e) -> Error(DecodingError(e))


let print_table_directory (common, _) =
  let tables = D.tables common in
  Printf.printf "tables:\n";
  tables |> List.iter (fun tag ->
    Printf.printf "- %s\n" (V.Tag.to_string tag)
  )


let print_cmap (common, _) =
  Printf.printf "cmap:\n";
  let res =
    let open ResultMonad in
    D.cmap common >>= fun icmap ->
    D.Intermediate.Cmap.get_subtables icmap >>= fun subtables ->
    subtables |> List.iter (fun subtable ->
      let ids = D.Intermediate.Cmap.get_subtable_ids subtable in
      Printf.printf "- subtable (platform: %d, encoding: %d, format: %d)\n"
        ids.platform_id
        ids.encoding_id
        ids.format;
      D.Intermediate.Cmap.fold_subtable subtable (fun () seg ->
        match seg with
        | D.Incremental(uch1, uch2, gid) ->
            if Uchar.equal uch1 uch2 then
              Printf.printf "  - I U+%04X --> %d\n" (Uchar.to_int uch1) gid
            else
              Printf.printf "  - I U+%04X, U+%04X --> %d\n" (Uchar.to_int uch1) (Uchar.to_int uch2) gid

        | D.Constant(uch1, uch2, gid) ->
            Printf.printf "  - C U+%04X, U+%04X --> %d\n" (Uchar.to_int uch1) (Uchar.to_int uch2) gid
      ) () |> ignore;
    );
    return ()
  in
  res |> inj


let print_head (common, _) =
  let res =
    let open ResultMonad in
    Printf.printf "head:\n";
    D.head common >>= fun head ->
    Format.printf "%a\n" V.Head.pp head;
    return ()
  in
  res |> inj


let print_hhea (common, _) =
  let res =
    let open ResultMonad in
    Printf.printf "hhea:\n";
    D.hhea common >>= fun hhea ->
    Format.printf "%a\n" V.Hhea.pp hhea;
    return ()
  in
  res |> inj


let print_maxp (common, _) =
  let open D in
  let res =
    let open ResultMonad in
    Printf.printf "maxp:\n";
    maxp common >>= fun maxp ->
    Format.printf "%a\n" V.Maxp.pp maxp;
    return ()
  in
  res |> inj


let write_glyph_svg path ~data =
  let open ResultMonad in
  try
    Core_kernel.Out_channel.write_all path ~data;
    return ()
  with
  | _ ->
      err @@ CannotWriteFile(path)


let print_glyf (common, specific) (gid : V.glyph_id) (path : string) =
  let open ResultMonad in
  Printf.printf "glyf (glyph ID: %d):\n" gid;
  match specific with
  | D.Cff(_) ->
      Printf.printf "  not a TTF\n";
      return ()

  | D.Ttf(ttf) ->
      D.loca ttf gid |> inj >>= function
      | None ->
          Printf.printf "  not defined\n";
          return ()

      | Some(loc) ->
          let res =
            D.head common >>= fun head ->
            D.glyf ttf loc >>= fun (descr, bbox) ->
            let data = Svg.make descr ~bbox ~units_per_em:head.V.Head.units_per_em in
            Format.printf "  (%a, %a)\n"
              V.pp_ttf_glyph_description descr
              V.pp_bounding_box bbox;
            return data
          in
          res |> inj >>= fun data ->
          write_glyph_svg path ~data


let pp_sep ppf () =
  Format.fprintf ppf ",@ "


let print_cff (_common, specific) (gid : V.glyph_id) =
  let open ResultMonad in
  Printf.printf "CFF (glyph ID: %d):\n" gid;
  match specific with
  | D.Ttf(_) ->
      Printf.printf "  not a CFF\n";
      return ()

  | D.Cff(cff) ->
      D.charstring cff gid |> inj >>= function
      | None ->
          Printf.printf "  not defined\n";
          return ()

      | Some((wopt, charstring)) ->
          begin
            match wopt with
            | None    -> Printf.printf "  width: not defined\n";
            | Some(w) -> Printf.printf "  width: %d\n" w;
          end;
          Format.printf "%a\n" D.pp_charstring charstring;
          D.path_of_charstring charstring |> inj >>= fun paths ->
          Format.printf "%a\n" (Format.pp_print_list ~pp_sep V.pp_cubic_path) paths;
          return ()


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
          let path = Sys.argv.(i + 2) in
          aux n { acc with glyf = Alist.extend acc.glyf (gid, path) } (i + 3)

      | "cff" ->
          let gid = int_of_string (Sys.argv.(i + 1)) in
          aux n { acc with cff = Alist.extend acc.cff gid } (i + 2)

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
        cff  = Alist.empty;
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
    D.source_of_string s |> inj >>= function
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
        config.glyf |> Alist.to_list |> mapM (fun (gid, path) ->
          print_glyf source gid path
        ) >>= fun _ ->
        config.cff |> Alist.to_list |> mapM (fun gid ->
          print_cff source gid
        ) >>= fun _ ->
        return ()

    | Collection(sources) ->
        sources |> List.iter print_table_directory;
        return ()
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> Format.printf "%a\n" pp_error e
