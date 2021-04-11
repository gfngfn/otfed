
module Alist = Otfed.Alist
module ResultMonad = Otfed.ResultMonad
module D = Otfed.Decode
module E = Otfed.Encode
module V = Otfed.Value
module I = Otfed.Intermediate
module Subset = Otfed.Subset

type config = {
  tables : bool;
  cmap   : bool;
  head   : bool;
  hhea   : bool;
  maxp   : bool;
  os2    : bool;
  math   : bool;
  kern   : bool;
  post   : bool;
  name   : bool;
  hmtx   : V.glyph_id Alist.t;
  glyf   : (V.glyph_id * string) Alist.t;
  cff    : (V.glyph_id * string) Alist.t;
  gsub   : (string * string * string) Alist.t;
  gpos   : (string * string * string) Alist.t;
  subset : (V.glyph_id list * string) Alist.t;
}

type error =
  | UnknownCommand     of string
  | InvalidCommandLine
  | CannotReadFile     of string
  | CannotWriteFile    of string
  | DecodingError      of D.Error.t
  | SubsetError        of Subset.error
[@@deriving show { with_path = false }]


let inj x =
  x |> Result.map_error (fun e -> DecodingError(e))

let inj_subset x =
  x |> Result.map_error (fun e -> SubsetError(e))


let print_table_directory (source : D.source) =
  let tables = D.tables source in
  Format.printf "tables:@,";
  tables |> List.iter (fun tag ->
    Format.printf "- %s@," (V.Tag.to_string tag)
  )


let print_cmap (source : D.source) =
  Format.printf "cmap:@,";
  let res =
    let open ResultMonad in
    D.Cmap.get source >>= fun icmap ->
    D.Cmap.get_subtables icmap >>= fun subtables ->
    subtables |> List.iter (fun subtable ->
      let ids = D.Cmap.get_subtable_ids subtable in
      let format = D.Cmap.get_format_number subtable in
      Format.printf "- subtable (platform: %d, encoding: %d, format: %d)@,"
        ids.platform_id
        ids.encoding_id
        format;
      D.Cmap.fold_subtable subtable (fun () seg ->
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


let print_hmtx (source : D.source) (gid : V.glyph_id) =
  Format.printf "hmtx (gid: %d):@," gid;
  let res =
    let open ResultMonad in
    D.Hmtx.get source >>= fun ihmtx ->
    D.Hmtx.access ihmtx gid >>= function
    | None ->
        Format.printf "- none@,";
        return ()

    | Some((aw, lsb)) ->
        Format.printf "- aw: %d, lsb: %d@,"
          aw lsb;
        return ()
  in
  res |> inj


let print_head (source : D.source) =
  let res =
    let open ResultMonad in
    Format.printf "head:@,";
    D.Head.get source >>= fun head ->
    Format.printf "%a@," I.Head.pp head;
    return ()
  in
  res |> inj


let print_hhea (source : D.source) =
  let res =
    let open ResultMonad in
    Format.printf "hhea:@,";
    D.Hhea.get source >>= fun hhea ->
    Format.printf "%a@," I.Hhea.pp hhea;
    return ()
  in
  res |> inj


let print_maxp (source : D.source) =
  let res =
    let open ResultMonad in
    Format.printf "maxp:@,";
    match source with
    | Ttf(ttf) ->
        D.Ttf.Maxp.get ttf >>= fun maxp ->
        Format.printf "%a@," I.Ttf.Maxp.pp maxp;
        return ()

    | Cff(cff) ->
        D.Cff.Maxp.get cff >>= fun maxp ->
        Format.printf "%a@," I.Cff.Maxp.pp maxp;
        return ()
  in
  res |> inj


let print_os2 (source : D.source) =
  let res =
    let open ResultMonad in
    Format.printf "OS/2:@,";
    D.Os2.get source >>= fun os2 ->
    Format.printf "%a@," I.Os2.pp os2;
    return ()
  in
  res |> inj


let print_math (source : D.source) =
  let res =
    let open ResultMonad in
    Format.printf "MATH:@,";
    D.Math.get source >>= function
    | None ->
        Format.printf "  MATH table not found@,";
        return ()

    | Some(math) ->
        Format.printf "  %a@," V.Math.pp math;
        return ()
  in
  res |> inj


let print_kern (source : D.source) =
  let res =
    let open ResultMonad in
    Format.printf "kern:@,";
    D.Kern.get source >>= function
    | None ->
        Format.printf "  kern table not found@,";
        return ()

    | Some(ikern) ->
        ikern |> D.Kern.fold
          (fun () kern_info ->
            Format.printf "  kern_info: %a@,"
              D.Kern.pp_kern_info kern_info;
            (true, ())
          )
          (fun () gid_left gid_right value ->
            Format.printf "  - (%d, %d): %d@,"
              gid_left gid_right value;
            ()
          )
          ()
  in
  res |> inj


let print_post (source : D.source) =
  let res =
    let open ResultMonad in
    Format.printf "post:@,";
    D.Post.get source >>= fun post ->
    Format.printf "%a@," V.Post.pp post;
    return ()
  in
  res |> inj


let print_name (source : D.source) =
  let res =
    let open ResultMonad in
    Format.printf "name:@,";
    D.Name.get source >>= fun name ->
    Format.printf "%a@," V.Name.pp name;
    return ()
  in
  res |> inj


let write_file path ~data =
  let open ResultMonad in
  try
    Core_kernel.Out_channel.write_all path ~data;
    return ()
  with
  | _ ->
      err @@ CannotWriteFile(path)


let print_glyf (source : D.source) (gid : V.glyph_id) (path : string) =
  let open ResultMonad in
  Format.printf "glyf (glyph ID: %d):@," gid;
  match source with
  | D.Cff(_) ->
      Format.printf "  not a TTF@,";
      return ()

  | D.Ttf(ttf) ->
      D.Ttf.loca ttf gid |> inj >>= function
      | None ->
          Format.printf "  not defined@,";
          return ()

      | Some(loc) ->
          D.Head.get source |> inj >>= fun head ->
          let units_per_em = head.I.Head.value.V.Head.units_per_em in
          D.Hmtx.get source |> inj >>= fun ihmtx ->
          D.Hmtx.access ihmtx gid |> inj >>= function
          | None ->
              Format.printf "  no hmtx entry@,";
              return ()

          | Some(aw, _lsb) ->
              D.Ttf.glyf ttf loc |> inj >>= fun { description = descr; bounding_box = bbox } ->
              Svg.make_ttf descr ~bbox ~units_per_em ~aw |> inj >>= fun data ->
              Format.printf "  (%a, %a)@,"
                V.pp_ttf_glyph_description descr
                V.pp_bounding_box bbox;
              write_file path ~data


let pp_sep ppf () =
  Format.fprintf ppf ", "


let pp_list pp =
  Format.pp_print_list ~pp_sep pp


let print_cff (source : D.source) (gid : V.glyph_id) (path : string) =
  let open ResultMonad in
  Format.printf "CFF (glyph ID: %d):@," gid;
  match source with
  | D.Ttf(_) ->
      Format.printf "  not a CFF@,";
      return ()

  | D.Cff(cff) ->
      D.Cff.charstring cff gid |> inj >>= function
      | None ->
          Format.printf "  not defined@,";
          return ()

      | Some((wopt, charstring)) ->
          begin
            match wopt with
            | None    -> Format.printf "  width: not defined@,";
            | Some(w) -> Format.printf "  width: %d@," w;
          end;
          D.Head.get source |> inj >>= fun head ->
          let units_per_em = head.I.Head.value.V.Head.units_per_em in
          D.Hmtx.get source |> inj >>= fun ihmtx ->
          D.Hmtx.access ihmtx gid |> inj >>= function
          | None ->
              Format.printf "  no hmtx entry@,";
              return ()

          | Some(aw, _lsb) ->
              Format.printf "%a@," D.pp_charstring charstring;
              D.Cff.path_of_charstring charstring |> inj >>= fun paths ->
              Format.printf "%a@," (pp_list V.pp_cubic_path) paths;
              let data = Svg.make_cff ~units_per_em paths ~aw in
              write_file path ~data


let print_gsub_feature (feature : D.Gsub.feature) =
  D.Gsub.fold_subtables
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


let print_gsub_langsys (langsys : D.Gsub.langsys) (feature_tag : string) =
  let open ResultMonad in
  D.Gsub.features langsys >>= fun (default_feature_opt, features) ->
  let features =
    match default_feature_opt with
    | None                  -> features
    | Some(default_feature) -> default_feature :: features
  in
  match
    features |> List.find_opt (fun feature -> String.equal feature_tag (D.Gsub.get_feature_tag feature))
  with
  | None ->
      Format.printf "  feature %s not found in:@," feature_tag;
      features |> List.iter (fun feature -> Format.printf "  - %s@," (D.Gsub.get_feature_tag feature));
      return ()

  | Some(feature) ->
      print_gsub_feature feature


let print_gsub_script (script : D.Gsub.script) (langsys_tag : string) (feature_tag : string) =
  let open ResultMonad in
  D.Gsub.langsyses script >>= fun (default_langsys_opt, langsyses) ->
  let langsyses =
    match default_langsys_opt with
    | None                  -> langsyses
    | Some(default_langsys) -> default_langsys :: langsyses
  in
  match
    langsyses |> List.find_opt (fun langsys -> String.equal langsys_tag (D.Gsub.get_langsys_tag langsys))
  with
  | None ->
      Format.printf "  langsys %s not found in:@," langsys_tag;
      langsyses |> List.iter (fun langsys -> Format.printf "  - %s@," (D.Gsub.get_langsys_tag langsys));
      return ()

  | Some(langsys) ->
      print_gsub_langsys langsys feature_tag


let print_gsub (source : D.source) (script_tag : string) (langsys_tag : string) (feature_tag : string) =
  let open ResultMonad in
  Format.printf "GSUB (script: %s, langsys: %s, feature: %s)@," script_tag langsys_tag feature_tag;
  D.Gsub.get source >>= function
  | None ->
      Format.printf "  GSUB table not found@,";
      return ()

  | Some(igsub) ->
      D.Gsub.scripts igsub >>= fun scripts ->
      match
        scripts |> List.find_opt (fun script -> String.equal script_tag (D.Gsub.get_script_tag script))
      with
      | None ->
          Format.printf "  script %s not found in:@," script_tag;
          scripts |> List.iter (fun script -> Format.printf "  - %s@," (D.Gsub.get_script_tag script));
          return ()

      | Some(script) ->
          print_gsub_script script langsys_tag feature_tag


let print_gpos_feature (feature : D.Gpos.feature) =
  D.Gpos.fold_subtables
    ~single1:(fun () gids _vr ->
      Format.printf "  - single1: {%a} --> ...@,"
        (pp_list Format.pp_print_int) gids
    )
    ~single2:(fun () (gid, _vr) ->
      Format.printf "  - single2: %d --> ...@,"
        gid
    )
    ~pair1:(fun () (gid, tos) ->
      Format.printf "  - pair1: %d --> {%a}@,"
        gid
        (pp_list Format.pp_print_int) (tos |> List.map (fun (gid, _vr1, _vr) -> gid))
    )
    ~pair2:(fun cdef1 cdef2 () assoc ->
      Format.printf "  - pair2: cdef1 = {%a}, cdef2 = {%a},@,"
        (pp_list D.Gpos.pp_class_definition) cdef1
        (pp_list D.Gpos.pp_class_definition) cdef2;
      assoc |> List.iter (fun (cv1, tos) ->
        Format.printf "    * %d --> {%a}@,"
          cv1
          (pp_list Format.pp_print_int) (tos |> List.map (fun (cv2, _, _) -> cv2))
      )
    )
    ~markbase1:(fun _class_count () mark_assoc base_assoc ->
      Format.printf "  - markbase1: mark = {%a}, base = {%a}@,"
        (pp_list Format.pp_print_int) (mark_assoc |> List.map (fun (gid, _) -> gid))
        (pp_list Format.pp_print_int) (base_assoc |> List.map (fun (gid, _) -> gid))
    )
    ~marklig1:(fun _class_count () mark_assoc lig_assoc ->
      Format.printf "  - marklig1: mark = {%a}, lig = {%a}@,"
        (pp_list Format.pp_print_int) (mark_assoc |> List.map (fun (gid, _) -> gid))
        (pp_list Format.pp_print_int) (lig_assoc |> List.map (fun (gid, _) -> gid))
    )
    ~markmark1:(fun _class_count () mark_assoc mark2_assoc ->
      Format.printf "  - markmark1: mark = {%a}, mark2 = {%a}@,"
        (pp_list Format.pp_print_int) (mark_assoc |> List.map (fun (gid, _) -> gid))
        (pp_list Format.pp_print_int) (mark2_assoc |> List.map (fun (gid, _) -> gid))
    )
    feature ()


let print_gpos_langsys (langsys : D.Gpos.langsys) (feature_tag : string) =
  let open ResultMonad in
  D.Gpos.features langsys >>= fun (default_feature_opt, features) ->
  let features =
    match default_feature_opt with
    | None                  -> features
    | Some(default_feature) -> default_feature :: features
  in
  match
    features |> List.find_opt (fun feature -> String.equal feature_tag (D.Gpos.get_feature_tag feature))
  with
  | None ->
      Format.printf "  feature %s not found in:@," feature_tag;
      features |> List.iter (fun feature -> Format.printf "  - %s@," (D.Gpos.get_feature_tag feature));
      return ()

  | Some(feature) ->
      print_gpos_feature feature


let print_gpos_script (script : D.Gpos.script) (langsys_tag : string) (feature_tag : string) =
  let open ResultMonad in
  D.Gpos.langsyses script >>= fun (default_langsys_opt, langsyses) ->
  let langsyses =
    match default_langsys_opt with
    | None                  -> langsyses
    | Some(default_langsys) -> default_langsys :: langsyses
  in
  match
    langsyses |> List.find_opt (fun langsys -> String.equal langsys_tag (D.Gpos.get_langsys_tag langsys))
  with
  | None ->
      Format.printf "  langsys %s not found in:@," langsys_tag;
      langsyses |> List.iter (fun langsys -> Format.printf "  - %s@," (D.Gpos.get_langsys_tag langsys));
      return ()

  | Some(langsys) ->
      print_gpos_langsys langsys feature_tag


let print_gpos (source : D.source) (script_tag : string) (langsys_tag : string) (feature_tag : string) =
  let open ResultMonad in
  Format.printf "GPOS (script: %s, langsys: %s, feature: %s)@," script_tag langsys_tag feature_tag;
  D.Gpos.get source >>= function
  | None ->
      Format.printf "  GPOS table not found@,";
      return ()

  | Some(igpos) ->
      D.Gpos.scripts igpos >>= fun scripts ->
      match
        scripts |> List.find_opt (fun script -> String.equal script_tag (D.Gpos.get_script_tag script))
      with
      | None ->
          Format.printf "  script %s not found in:@," script_tag;
          scripts |> List.iter (fun script -> Format.printf "  - %s@," (D.Gpos.get_script_tag script));
          return ()

      | Some(script) ->
          print_gpos_script script langsys_tag feature_tag


let make_subset (source : D.source) (gids : V.glyph_id list) (path : string) =
  let open ResultMonad in
  Subset.make source gids |> inj_subset >>= fun data ->
  write_file path ~data


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
      | "os2"    -> aux n { acc with os2 = true } (i + 1)
      | "math"   -> aux n { acc with math = true } (i + 1)
      | "kern"   -> aux n { acc with kern = true } (i + 1)
      | "post"   -> aux n { acc with post = true } (i + 1)
      | "name"   -> aux n { acc with name = true } (i + 1)

      | "hmtx" ->
          let gid = int_of_string (Sys.argv.(i + 1)) in
          aux n { acc with hmtx = Alist.extend acc.hmtx gid } (i + 2)

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

      | "gpos" ->
          let script  = Sys.argv.(i + 1) in
          let langsys = Sys.argv.(i + 2) in
          let feature = Sys.argv.(i + 3) in
          aux n { acc with gpos = Alist.extend acc.gpos (script, langsys, feature) } (i + 4)

      | "subset" ->
          let gids = String.split_on_char ',' Sys.argv.(i + 1) |> List.map int_of_string in
          let path = Sys.argv.(i + 2) in
          aux n { acc with subset = Alist.extend acc.subset (gids, path) } (i + 3)

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
        os2    = false;
        math   = false;
        kern   = false;
        post   = false;
        name   = false;
        hmtx   = Alist.empty;
        glyf   = Alist.empty;
        cff    = Alist.empty;
        gsub   = Alist.empty;
        gpos   = Alist.empty;
        subset = Alist.empty;
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
          if config.os2 then print_os2 source else return ()
        end >>= fun () ->
        begin
          if config.math then print_math source else return ()
        end >>= fun () ->
        begin
          if config.kern then print_kern source else return ()
        end >>= fun () ->
        begin
          if config.post then print_post source else return ()
        end >>= fun () ->
        begin
          if config.name then print_name source else return ()
        end >>= fun () ->
        config.hmtx |> Alist.to_list |> mapM (fun gid ->
          print_hmtx source gid
        ) >>= fun _ ->
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
        config.gpos |> Alist.to_list |> mapM (fun (script, langsys, feature) ->
          print_gpos source script langsys feature |> inj
        ) >>= fun _ ->
        config.subset |> Alist.to_list |> mapM (fun (gids, path) ->
          make_subset source gids path
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
