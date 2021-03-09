
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
  cff  : (V.glyph_id * string) Alist.t;
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
  Format.printf "tables:@,";
  tables |> List.iter (fun tag ->
    Format.printf "- %s@," (V.Tag.to_string tag)
  )


let print_cmap (common, _) =
  Format.printf "cmap:@,";
  let res =
    let open ResultMonad in
    D.cmap common >>= fun icmap ->
    D.Intermediate.Cmap.get_subtables icmap >>= fun subtables ->
    subtables |> List.iter (fun subtable ->
      let ids = D.Intermediate.Cmap.get_subtable_ids subtable in
      Format.printf "- subtable (platform: %d, encoding: %d, format: %d)@,"
        ids.platform_id
        ids.encoding_id
        ids.format;
      D.Intermediate.Cmap.fold_subtable subtable (fun () seg ->
        match seg with
        | D.Incremental(uch1, uch2, gid) ->
            if Uchar.equal uch1 uch2 then
              Format.printf "  - I U+%04X --> %d@," (Uchar.to_int uch1) gid
            else
              Format.printf "  - I U+%04X, U+%04X --> %d@," (Uchar.to_int uch1) (Uchar.to_int uch2) gid

        | D.Constant(uch1, uch2, gid) ->
            Format.printf "  - C U+%04X, U+%04X --> %d@," (Uchar.to_int uch1) (Uchar.to_int uch2) gid
      ) () |> ignore;
    );
    return ()
  in
  res |> inj


let print_head (common, _) =
  let res =
    let open ResultMonad in
    Format.printf "head:@,";
    D.head common >>= fun head ->
    Format.printf "%a@," V.Head.pp head;
    return ()
  in
  res |> inj


let print_hhea (common, _) =
  let res =
    let open ResultMonad in
    Format.printf "hhea:@,";
    D.hhea common >>= fun hhea ->
    Format.printf "%a@," V.Hhea.pp hhea;
    return ()
  in
  res |> inj


let print_maxp (common, _) =
  let open D in
  let res =
    let open ResultMonad in
    Format.printf "maxp:@,";
    maxp common >>= fun maxp ->
    Format.printf "%a@," V.Maxp.pp maxp;
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
  Format.printf "glyf (glyph ID: %d):@," gid;
  match specific with
  | D.Cff(_) ->
      Format.printf "  not a TTF@,";
      return ()

  | D.Ttf(ttf) ->
      D.loca ttf gid |> inj >>= function
      | None ->
          Format.printf "  not defined@,";
          return ()

      | Some(loc) ->
          let res =
            D.head common >>= fun head ->
            D.glyf ttf loc >>= fun (descr, bbox) ->
            Svg.make_ttf descr ~bbox ~units_per_em:head.V.Head.units_per_em >>= fun data ->
            Format.printf "  (%a, %a)@,"
              V.pp_ttf_glyph_description descr
              V.pp_bounding_box bbox;
            return data
          in
          res |> inj >>= fun data ->
          write_glyph_svg path ~data


let pp_sep ppf () =
  Format.fprintf ppf ",@ "


let print_cff (common, specific) (gid : V.glyph_id) (path : string) =
  let open ResultMonad in
  Format.printf "CFF (glyph ID: %d):@," gid;
  match specific with
  | D.Ttf(_) ->
      Format.printf "  not a CFF@,";
      return ()

  | D.Cff(cff) ->
      D.head common |> inj >>= fun head ->
      D.charstring cff gid |> inj >>= function
      | None ->
          Format.printf "  not defined@,";
          return ()

      | Some((wopt, charstring)) ->
          begin
            match wopt with
            | None    -> Format.printf "  width: not defined@,";
            | Some(w) -> Format.printf "  width: %d@," w;
          end;
          Format.printf "%a@," D.pp_charstring charstring;
          D.path_of_charstring charstring |> inj >>= fun paths ->
          Format.printf "%a@," (Format.pp_print_list ~pp_sep V.pp_cubic_path) paths;
          let data = Svg.make_cff ~units_per_em:head.V.Head.units_per_em paths in
          write_glyph_svg path ~data


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
          let path = Sys.argv.(i + 2) in
          aux n { acc with cff = Alist.extend acc.cff (gid, path) } (i + 3)

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
    Format.printf "@[<v>";
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
        config.cff |> Alist.to_list |> mapM (fun (gid, path) ->
          print_cff source gid path
        ) >>= fun _ ->
        return ()

    | Collection(sources) ->
        sources |> List.iter print_table_directory;
        return ()
  in
  Format.printf "@]";
  match res with
  | Ok(())   -> ()
  | Error(e) -> Format.printf "%a@," pp_error e
