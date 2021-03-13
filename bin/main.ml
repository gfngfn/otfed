
module Alist = Otfed.Alist
module ResultMonad = Otfed.ResultMonad
module D = Otfed.Decode
module DGsub = D.Intermediate.Gsub
module V = Otfed.Value

type config = {
  tables : bool;
  cmap   : bool;
  head   : bool;
  hhea   : bool;
  maxp   : bool;
  glyf   : (V.glyph_id * string) Alist.t;
  cff    : (V.glyph_id * string) Alist.t;
  gsub   : (string * string * string) Alist.t;
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
  Format.fprintf ppf ", "


let pp_list pp =
  Format.pp_print_list ~pp_sep pp


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
          Format.printf "%a@," (pp_list V.pp_cubic_path) paths;
          let data = Svg.make_cff ~units_per_em:head.V.Head.units_per_em paths in
          write_glyph_svg path ~data


let print_gsub_feature (feature : DGsub.feature) =
  DGsub.fold_subtables
    ~single:(fun () (gid_from, gid_to) ->
      Format.printf "  - single: %d --> %d@," gid_from gid_to;
    )
    ~alt:(fun () (gid_from, gids_to) ->
      Format.printf "  - alt: %d --> {%a}@," gid_from (pp_list Format.pp_print_int) gids_to
    )
    ~lig:(fun () (gid_from, tos) ->
      Format.printf "  - lig: %d -->@," gid_from;
      tos |> List.iter (fun (gids_tail, gid_to) ->
        Format.printf "    * {%a} --> %d@," (pp_list Format.pp_print_int) gids_tail gid_to
      )
    )
    feature ()


let print_gsub_langsys (langsys : DGsub.langsys) (feature_tag : string) =
  let open ResultMonad in
  DGsub.features langsys >>= fun (default_feature_opt, features) ->
  let features =
    match default_feature_opt with
    | None                  -> features
    | Some(default_feature) -> default_feature :: features
  in
  match
    features |> List.find_opt (fun feature -> String.equal feature_tag (DGsub.get_feature_tag feature))
  with
  | None ->
      Format.printf "  feature %s not found in:@," feature_tag;
      features |> List.iter (fun feature -> Format.printf "  - %s@," (DGsub.get_feature_tag feature));
      return ()

  | Some(feature) ->
      print_gsub_feature feature


let print_gsub_script (script : DGsub.script) (langsys_tag : string) (feature_tag : string) =
  let open ResultMonad in
  DGsub.langsyses script >>= fun (default_langsys_opt, langsyses) ->
  let langsyses =
    match default_langsys_opt with
    | None                  -> langsyses
    | Some(default_langsys) -> default_langsys :: langsyses
  in
  match
    langsyses |> List.find_opt (fun langsys -> String.equal langsys_tag (DGsub.get_langsys_tag langsys))
  with
  | None ->
      Format.printf "  langsys %s not found in:@," langsys_tag;
      langsyses |> List.iter (fun langsys -> Format.printf "  - %s@," (DGsub.get_langsys_tag langsys));
      return ()

  | Some(langsys) ->
      print_gsub_langsys langsys feature_tag


let print_gsub (common, _) (script_tag : string) (langsys_tag : string) (feature_tag : string) =
  let open ResultMonad in
  Format.printf "GSUB (script: %s, langsys: %s, feature: %s)@," script_tag langsys_tag feature_tag;
  D.gsub common >>= function
  | None ->
      Format.printf "  GSUB table not found@,";
      return ()

  | Some(igsub) ->
      DGsub.scripts igsub >>= fun scripts ->
      match
        scripts |> List.find_opt (fun script -> String.equal script_tag (DGsub.get_script_tag script))
      with
      | None ->
          Format.printf "  script %s not found in:@," script_tag;
          scripts |> List.iter (fun script -> Format.printf "  - %s@," (DGsub.get_script_tag script));
          return ()

      | Some(script) ->
          print_gsub_script script langsys_tag feature_tag


let parse_args () =
  let open ResultMonad in
  let rec aux n acc i =
    if i >= n then
      return acc
    else
      match Sys.argv.(i) with
      | "tables" -> aux n { acc with tables = true } (i + 1)
      | "cmap"   -> aux n { acc with cmap = true } (i + 1)
      | "head"   -> aux n { acc with head = true } (i + 1)
      | "hhea"   -> aux n { acc with hhea = true } (i + 1)
      | "maxp"   -> aux n { acc with maxp = true } (i + 1)

      | "glyf" ->
          let gid = int_of_string (Sys.argv.(i + 1)) in
          let path = Sys.argv.(i + 2) in
          aux n { acc with glyf = Alist.extend acc.glyf (gid, path) } (i + 3)

      | "cff" ->
          let gid = int_of_string (Sys.argv.(i + 1)) in
          let path = Sys.argv.(i + 2) in
          aux n { acc with cff = Alist.extend acc.cff (gid, path) } (i + 3)

      | "gsub" ->
          let script  = Sys.argv.(i + 1) in
          let langsys = Sys.argv.(i + 2) in
          let feature = Sys.argv.(i + 3) in
          aux n { acc with gsub = Alist.extend acc.gsub (script, langsys, feature) } (i + 4)

      | s ->
          err @@ UnknownCommand(s)
  in
  try
    let n = Array.length Sys.argv in
    let path = Sys.argv.(1) in
    let config =
      {
        tables = false;
        cmap   = false;
        head   = false;
        hhea   = false;
        maxp   = false;
        glyf   = Alist.empty;
        cff    = Alist.empty;
        gsub   = Alist.empty;
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
        if config.tables then print_table_directory source else ();
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
        config.gsub |> Alist.to_list |> mapM (fun (script, langsys, feature) ->
          print_gsub source script langsys feature |> inj
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
