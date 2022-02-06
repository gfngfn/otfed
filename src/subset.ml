
open Basic
open Value


type error =
  | NoGlyphGiven
  | GlyphNotFound of glyph_id
  | DependentGlyphNotFound of { depending : glyph_id; depended : glyph_id }
  | DecodeError   of DecodeError.t
  | EncodeError   of EncodeError.t
  | NonexplicitSubroutineNumber
[@@deriving show { with_path = false }]

type 'a ok = ('a, error) result


let inj_dec d = Result.map_error (fun e -> DecodeError(e)) d
let inj_enc e = Result.map_error (fun e -> EncodeError(e)) e


let get_ttf_glyph (ttf : Decode.ttf_source) (gid : glyph_id) : Ttf.glyph_info ok =
  let open ResultMonad in
  inj_dec @@ Decode.Ttf.loca ttf gid >>= function
  | None      -> err @@ GlyphNotFound(gid)
  | Some(loc) -> inj_dec @@ Decode.Ttf.glyf ttf loc


type glyph_accumulator = (glyph_id * Ttf.glyph_info) Alist.t * bounding_box


let folding_ttf_glyph (ttf : Decode.ttf_source) ((ggs, bbox_all) : glyph_accumulator) (gid : glyph_id) : glyph_accumulator ok =
  let open ResultMonad in
  get_ttf_glyph ttf gid >>= fun g ->
  return (Alist.extend ggs (gid, g), unite_bounding_boxes bbox_all g.bounding_box)


let get_ttf_glyphs (ttf : Decode.ttf_source) (gids : glyph_id list) : ((glyph_id * Ttf.glyph_info) list * bounding_box) ok =
  let open ResultMonad in
  match gids with
  | [] ->
      err NoGlyphGiven

  | gid_first :: gids_tail ->
      get_ttf_glyph ttf gid_first >>= fun g_first ->
      foldM (folding_ttf_glyph ttf) gids_tail (Alist.empty, g_first.bounding_box) >>= fun (gs_tail, bbox_all) ->
      let gs = (gid_first, g_first) :: Alist.to_list gs_tail in
      return (gs, bbox_all)


module OldToNew = Map.Make(Int)


let edit_composite_glyphs (ggs : (glyph_id * Ttf.glyph_info) list) =
  let open ResultMonad in
  let (_, old_to_new) =
    ggs |> List.fold_left (fun (gid_new, old_to_new) (gid_old, _) ->
      (gid_new + 1, old_to_new |> OldToNew.add gid_old gid_new)
    ) (0, OldToNew.empty)
  in
  ggs |> mapM (fun gg_old ->
    let (gid_old, g_old) = gg_old in
    let Ttf.{ bounding_box = bbox; description = descr_old } = g_old in
    match descr_old with
    | Ttf.SimpleGlyph(_) ->
        return gg_old

    | Ttf.CompositeGlyph(composite_elems_old) ->
        composite_elems_old |> mapM (fun (gid_elem_old, composition, linear_opt) ->
          match old_to_new |> OldToNew.find_opt gid_elem_old with
          | None ->
              err @@ DependentGlyphNotFound{ depending = gid_old; depended = gid_elem_old }

          | Some(gid_elem_new) ->
              return (gid_elem_new, composition, linear_opt)
        ) >>= fun descr_new ->
        return (gid_old, Ttf.{ bounding_box = bbox; description = Ttf.CompositeGlyph(descr_new) })
  )


type hmtx_entry = design_units * design_units

type hmtx_accumulator = {
  current_hhea_derived : Intermediate.Hhea.derived;
  hmtx_entries         : hmtx_entry Alist.t;
  advance_width_sum    : wint;
  nonzero_width_count  : int;
}

type hmtx_result = {
  table_hmtx          : Encode.table;
  hhea_derived        : Intermediate.Hhea.derived;
  number_of_h_metrics : int;
  average_char_width  : design_units;
}


let update_derived ~current:derived ~new_one:derived_new =
  Intermediate.Hhea.{
    advance_width_max      = Stdlib.max derived.advance_width_max      derived_new.advance_width_max;
    min_left_side_bearing  = Stdlib.min derived.min_left_side_bearing  derived_new.min_left_side_bearing;
    min_right_side_bearing = Stdlib.min derived.min_right_side_bearing derived_new.min_right_side_bearing;
    xmax_extent            = Stdlib.max derived.xmax_extent            derived_new.xmax_extent;
  }


let get_ttf_hmtx (ihmtx : Decode.Hmtx.t) ((gid, g) : glyph_id * Ttf.glyph_info) : (Intermediate.Hhea.derived * hmtx_entry * design_units) ok =
  let open ResultMonad in
  inj_dec @@ Decode.Hmtx.access ihmtx gid >>= function
  | None ->
      err @@ GlyphNotFound(gid)

  | Some((aw, lsb) as entry) ->
      let derived =
        let x_min = g.bounding_box.x_min in
        let x_max = g.bounding_box.x_max in
        let extent = x_max - x_min in
        let rsb = aw - lsb - extent in
        Intermediate.Hhea.{
          advance_width_max      = aw;
          min_left_side_bearing  = lsb;
          min_right_side_bearing = rsb;
          xmax_extent            = extent;
        }
      in
      return (derived, entry, aw)


let folding_ttf_hmtx (ihmtx : Decode.Hmtx.t) (acc : hmtx_accumulator) (gg : glyph_id * Ttf.glyph_info) : hmtx_accumulator ok =
  let open ResultMonad in
  let derived = acc.current_hhea_derived in
  get_ttf_hmtx ihmtx gg >>= fun (derived_new, entry, aw) ->
      let derived = update_derived ~current:derived ~new_one:derived_new in
      return {
        current_hhea_derived = derived;
        hmtx_entries         = Alist.extend acc.hmtx_entries entry;
        advance_width_sum    = acc.advance_width_sum +% !% aw;
        nonzero_width_count  = if aw = 0 then acc.nonzero_width_count else acc.nonzero_width_count + 1;
      }


let make_ttf_hmtx (src : Decode.source) (ggs : (glyph_id * Ttf.glyph_info) list) : hmtx_result ok =
  let open ResultMonad in
  inj_dec @@ Decode.Hmtx.get src >>= fun ihmtx ->
  match ggs with
  | [] ->
      err NoGlyphGiven

  | gg_first :: ggs_tail ->
      get_ttf_hmtx ihmtx gg_first >>= fun (derived_first, entry_first, aw) ->
      let acc =
        {
          current_hhea_derived = derived_first;
          hmtx_entries         = Alist.empty;
          advance_width_sum    = !% aw;
          nonzero_width_count  = if aw = 0 then 0 else 1;
        }
      in
      foldM (folding_ttf_hmtx ihmtx) ggs_tail acc >>= fun acc ->
      let hhea_derived = acc.current_hhea_derived in
      let entries_tail = acc.hmtx_entries in
      inj_enc @@ Encode.Hmtx.make (entry_first :: Alist.to_list entries_tail) >>= fun table_hmtx ->
      let number_of_h_metrics = List.length ggs in
      let average_char_width =
        let count = acc.nonzero_width_count in
        if count = 0 then 0 else WideInt.to_int (acc.advance_width_sum /% (!% count))
      in
      return {
        table_hmtx;
        hhea_derived;
        number_of_h_metrics;
        average_char_width;
      }


let get_cff_hmtx (cff : Decode.cff_source) (ihmtx : Decode.Hmtx.t) (gid : glyph_id) : (Intermediate.Hhea.derived * hmtx_entry * design_units * Value.bounding_box) ok =
  let open ResultMonad in
  inj_dec @@ Decode.Hmtx.access ihmtx gid >>= function
  | None ->
      err @@ GlyphNotFound(gid)

  | Some((aw, lsb) as entry) ->
      inj_dec @@ Decode.Cff.charstring cff gid >>= function
      | None ->
          err @@ GlyphNotFound(gid)

      | Some(_, charstring) ->
          inj_dec @@ Decode.Cff.path_of_charstring charstring >>= fun cpaths ->
          let bbox =
            match calculate_bounding_box_of_paths cpaths with
            | None       -> Value.{ x_min = 0; y_min = 0; x_max = 0; y_max = 0 }
            | Some(bbox) -> bbox
          in
          let derived =
            let x_min = bbox.x_min in
            let x_max = bbox.x_max in
            let extent = x_max - x_min in
            let rsb = aw - lsb - extent in
            Intermediate.Hhea.{
              advance_width_max      = aw;
              min_left_side_bearing  = lsb;
              min_right_side_bearing = rsb;
              xmax_extent            = extent;
            }
          in
          return (derived, entry, aw, bbox)


type cff_hmtx_accumulator = hmtx_accumulator * Value.bounding_box


let folding_cff_hmtx (cff : Decode.cff_source) (ihmtx : Decode.Hmtx.t) ((acc, bbox) : cff_hmtx_accumulator) (gid : glyph_id) : cff_hmtx_accumulator ok =
  let open ResultMonad in
  let derived = acc.current_hhea_derived in
  get_cff_hmtx cff ihmtx gid >>= fun (derived_new, entry, aw, bbox_new) ->
  let derived = update_derived ~current:derived ~new_one:derived_new in
  let bbox = unite_bounding_boxes bbox bbox_new in
  return ({
    current_hhea_derived = derived;
    hmtx_entries         = Alist.extend acc.hmtx_entries entry;
    advance_width_sum    = acc.advance_width_sum +% !% aw;
    nonzero_width_count  = if aw = 0 then acc.nonzero_width_count else acc.nonzero_width_count + 1;
  }, bbox)


let make_cff_hmtx (cff : Decode.cff_source) (gids : glyph_id list) : (hmtx_result * Value.bounding_box) ok =
  let open ResultMonad in
  inj_dec @@ Decode.Hmtx.get (Cff(cff)) >>= fun ihmtx ->
  match gids with
  | [] ->
      err NoGlyphGiven

  | gid_first :: gids_tail ->
      get_cff_hmtx cff ihmtx gid_first >>= fun (derived_first, entry_first, aw, bbox) ->
      let acc =
        {
          current_hhea_derived = derived_first;
          hmtx_entries         = Alist.empty;
          advance_width_sum    = !% aw;
          nonzero_width_count  = if aw = 0 then 0 else 1;
        }
      in
      foldM (folding_cff_hmtx cff ihmtx) gids_tail (acc, bbox) >>= fun (acc, bbox) ->
      let hhea_derived = acc.current_hhea_derived in
      let entries_tail = acc.hmtx_entries in
      inj_enc @@ Encode.Hmtx.make (entry_first :: Alist.to_list entries_tail) >>= fun table_hmtx ->
      let number_of_h_metrics = List.length gids in
      let average_char_width =
        let count = acc.nonzero_width_count in
        if count = 0 then 0 else WideInt.to_int (acc.advance_width_sum /% (!% count))
      in
      return ({
        table_hmtx;
        hhea_derived;
        number_of_h_metrics;
        average_char_width;
      }, bbox)


let make_os2 (src : Decode.source) ~average_char_width ~first_char ~max_context ~last_char : Encode.table ok =
  let open ResultMonad in
  inj_dec @@ Decode.Os2.get src >>= fun { value = os2_value; _ } ->
  let os2_derived =
    Intermediate.Os2.{
      x_avg_char_width    = average_char_width;
      us_first_char_index = first_char;
      us_last_char_index  = last_char;
      us_max_context      = max_context;
    }
  in
  let ios2 =
    Intermediate.Os2.{
      value   = os2_value;
      derived = os2_derived;
    }
  in
  inj_enc @@ Encode.Os2.make ios2


module OldToNewMap = Map.Make(Int)

type old_to_new_map = glyph_id OldToNewMap.t


let make_subset_of_subtable ~current_min:(uch_min : Uchar.t) ~current_max:(uch_max : Uchar.t) (old_to_new_map : old_to_new_map) (subtable_old : Value.Cmap.subtable) : Value.Cmap.subtable * Uchar.t * Uchar.t =
  let open Value.Cmap in
  let mapping_old = subtable_old.mapping in
  let (mapping_new, uch_min, uch_max) =
    Mapping.fold (fun uch gid_old ((mapping_new, uch_min, uch_max) as acc) ->
      match old_to_new_map |> OldToNewMap.find_opt gid_old with
      | None ->
          acc

      | Some(gid_new) ->
          let mapping_new = mapping_new |> Mapping.add_single uch gid_new in
          (mapping_new, Stdlib.min uch_min uch, Stdlib.max uch_max uch)
    ) mapping_old (Mapping.empty, uch_min, uch_max)
  in
  let subtable_new =
    {
      subtable_ids = subtable_old.subtable_ids;
      mapping      = mapping_new;
    }
  in
  (subtable_new, uch_min, uch_max)


let make_cmap (src : Decode.source) (gids : glyph_id list) : (Encode.table * Uchar.t * Uchar.t) ok =
  let open ResultMonad in
  let res_dec =
    Decode.Cmap.get src >>= fun icmap ->
    Decode.Cmap.get_subtables icmap >>= fun isubtables ->
    isubtables |> mapM Decode.Cmap.unmarshal_subtable
  in
  let (old_to_new_map, _) =
    gids |> List.fold_left (fun (old_to_new_map, gid_new) gid_old ->
      (old_to_new_map |> OldToNewMap.add gid_old gid_new, gid_new + 1)
    ) (OldToNewMap.empty, 0)
  in
  inj_dec res_dec >>= fun subtables ->
  let (subtable_new_acc, uch_min, uch_max) =
    subtables |> List.fold_left (fun (subtable_new_acc, uch_min, uch_max) subtable_old ->
      let (subtable_new, uch_min, uch_max) =
        make_subset_of_subtable
          ~current_min:uch_min
          ~current_max:uch_max
          old_to_new_map
          subtable_old
      in
      (Alist.extend subtable_new_acc subtable_new, uch_min, uch_max)
    ) (Alist.empty, Uchar.of_int 0xFFFF, Uchar.of_int 0)
  in
  let subtables_new = subtable_new_acc |> Alist.to_list in
  inj_enc @@ Encode.Cmap.make subtables_new >>= fun table_cmap ->
  return (table_cmap, uch_min, uch_max)


let make_common
    ~(omit_cmap : bool)
    ~(bbox_all : bounding_box)
    ~(index_to_loc_format : Intermediate.loc_format)
    ~(hmtx_result : hmtx_result)
    (src : Decode.source) (gids : glyph_id list) =
  let open ResultMonad in

  (* Make `cmap`. *)
  make_cmap src gids >>= fun (table_cmap, first_char, last_char) ->

  (* Make `post`. *)
  inj_dec @@ Decode.Post.get src >>= fun post ->
  inj_enc @@ Encode.Post.make post >>= fun table_post ->

  (* Make `name`. *)
  inj_dec @@ Decode.Name.get src >>= fun name ->
  inj_enc @@ Encode.Name.make name >>= fun table_name ->

  let { table_hmtx; hhea_derived; number_of_h_metrics; average_char_width; } = hmtx_result in

  (* Make `OS/2`. *)
  let max_context = Some(1) in (* TODO: should extend this if this function can encode advanced tables. *)
  make_os2 src ~average_char_width ~first_char ~last_char ~max_context >>= fun table_os2 ->

  (* Make `hhea`. *)
  inj_dec @@ Decode.Hhea.get src >>= fun { value = hhea_value; _ } ->
  let ihhea =
    Intermediate.Hhea.{
      value   = hhea_value;
      derived = hhea_derived;
    }
  in
  inj_enc @@ Encode.Hhea.make ~number_of_h_metrics ihhea >>= fun table_hhea ->

  (* Make `head`. *)
  inj_dec @@ Decode.Head.get src >>= fun { value = head_value; _ } ->
  let head_derived =
    Intermediate.Head.{
      x_min = bbox_all.Value.x_min;
      y_min = bbox_all.Value.y_min;
      x_max = bbox_all.Value.x_max;
      y_max = bbox_all.Value.y_max;
      index_to_loc_format;
    }
  in
  let ihead =
    Intermediate.Head.{
      value   = head_value;
      derived = head_derived;
    }
  in
  inj_enc @@ Encode.Head.make ihead >>= fun table_head ->
  return @@ List.concat [
    [
      table_head;
      table_hhea;
      table_hmtx;
      table_post;
      table_name;
      table_os2;
    ];
    if omit_cmap then [] else [table_cmap];
  ]


let make_ttf_subset ~(omit_cmap : bool) (ttf : Decode.ttf_source) (gids : glyph_id list) : string ok =
  let open ResultMonad in

  let src = Decode.Ttf(ttf) in
  let num_glyphs = List.length gids in

  (* Make `glyf` and `loca`. *)
  get_ttf_glyphs ttf gids >>= fun (ggs, bbox_all) ->
  edit_composite_glyphs ggs >>= fun ggs ->
  inj_enc @@ Encode.Ttf.make_glyf (ggs |> List.map snd) >>= fun (table_glyf, locs) ->
  inj_enc @@ Encode.Ttf.make_loca locs >>= fun (table_loca, index_to_loc_format) ->

  (* Make `maxp` *)
  inj_dec @@ Decode.Ttf.Maxp.get ttf >>= fun maxp ->
  let maxp =
    { maxp with num_glyphs = num_glyphs }  (* TODO: set more accurate data *)
  in
  inj_enc @@ Encode.Ttf.Maxp.make maxp >>= fun table_maxp ->

  (* Make `hmtx` and get derived data for `hhea`. *)
  make_ttf_hmtx src ggs >>= fun hmtx_result ->

  make_common
    ~omit_cmap
    ~bbox_all
    ~index_to_loc_format
    ~hmtx_result
    src gids >>= fun common_tables ->

  inj_enc @@ Encode.make_font_data_from_tables ~ttf:true @@
    List.append common_tables [
      table_maxp;
      table_loca;
      table_glyf;
    ]


module Lsi = Decode.Cff.LexicalSubroutineIndex


module LsiMap = Map.Make(Int)


type local_subrs_info =
  | SingleLsubrs of Lsi.t
  | FDLsubrs     of Lsi.t LsiMap.t


module Old = struct

  type t =
    | Global of int
    | Local  of Intermediate.Cff.fdindex option * int


  let compare old1 old2 =
    match (old1, old2) with
    | (Global(i1), Global(i2)) ->
        Int.compare i1 i2

    | (Global(_), Local(_)) ->
        -1

    | (Local(_), Global(_)) ->
        1

    | (Local(fo1, i1), Local(fo2, i2)) ->
        begin
          match (fo1, fo2) with
          | (Some(f1), Some(f2)) ->
              let c = Int.compare f1 f2 in
              if c = 0 then Int.compare i1 i2 else c

          | (Some(_), None) -> -1
          | (None, Some(_)) -> 1
          | (None, None)    -> Int.compare i1 i2
        end
end


module RenumberMap = Map.Make(Old)


let renumber_subroutine ~(fdindex_opt : int option) ~(bias : int) (renumber_map : int RenumberMap.t) (tokens_old : Intermediate.Cff.lexical_charstring) =
  let open ResultMonad in
  let open Intermediate.Cff in
  let rec aux token_new_acc = function
    | [] ->
        return @@ Alist.to_list token_new_acc

    | ArgumentInteger(i_old) :: OpCallGSubr :: tokens ->
        let i_new_biased =
          match renumber_map |> RenumberMap.find_opt (Old.Global(i_old)) with
          | None        -> assert false
          | Some(i_new) -> i_new - bias
        in
        aux (Alist.append token_new_acc [ArgumentInteger(i_new_biased); OpCallGSubr]) tokens

    | _ :: OpCallGSubr :: _ ->
        err @@ NonexplicitSubroutineNumber

    | ArgumentInteger(i_old) :: OpCallSubr :: tokens ->
        let i_new_biased =
          match renumber_map |> RenumberMap.find_opt (Old.Local(fdindex_opt, i_old)) with
          | None        -> assert false
          | Some(i_new) -> i_new - bias
        in
        aux (Alist.append token_new_acc [ArgumentInteger(i_new_biased); OpCallGSubr]) tokens

    | _ :: OpCallSubr :: _ ->
        err @@ NonexplicitSubroutineNumber

    | token :: tokens ->
        aux (Alist.extend token_new_acc token) tokens
  in
  aux Alist.empty tokens_old


let get_glyph_name (cff : Decode.cff_source) (gid : glyph_id) : string ok =
  let open ResultMonad in
  inj_dec @@ Decode.Cff.access_charset cff gid >>= fun name_opt ->
  let name =
    match name_opt with
    | Some(name) -> name
    | None       -> Printf.sprintf "from%d" gid
  in
  return name


let make_cff ~(num_glyphs : int) (cff : Decode.cff_source) (gids : glyph_id list) =
  let open ResultMonad in
  inj_dec @@ Decode.Cff.top_dict cff >>= fun top_dict ->
  begin
    match gids with
    | [] ->
        err @@ NoGlyphGiven

    | gid :: _ ->
        begin
          inj_dec @@ Decode.Cff.fdindex cff gid >>= function
          | None    -> return @@ SingleLsubrs(Lsi.empty)
          | Some(_) -> return @@ FDLsubrs(LsiMap.empty)
        end
  end >>= fun lsubrs_info ->
  foldM (fun ((lcsacc, gsubrs, lsubrs_info) : (Intermediate.Cff.lexical_charstring * Intermediate.Cff.fdindex option * string) Alist.t * Lsi.t * local_subrs_info) (gid : glyph_id) ->
      get_glyph_name cff gid >>= fun name ->
      match lsubrs_info with
      | SingleLsubrs(lsubrs) ->
          begin
            inj_dec @@ Decode.Cff.fdindex cff gid >>= function
            | Some(_fdindex) ->
                assert false

            | None ->
                begin
                  inj_dec @@ Decode.Cff.lexical_charstring cff ~gsubrs ~lsubrs gid >>= function
                  | None ->
                      err @@ GlyphNotFound(gid)

                  | Some(gsubrs, lsubrs, lcs) ->
                      return (Alist.extend lcsacc (lcs, None, name), gsubrs, SingleLsubrs(lsubrs))
                end
          end

      | FDLsubrs(lsubrs_map) ->
          begin
            inj_dec @@ Decode.Cff.fdindex cff gid >>= function
            | None ->
                assert false

            | Some(fdindex) ->
                let lsubrs =
                  match lsubrs_map |> LsiMap.find_opt fdindex with
                  | None         -> Lsi.empty
                  | Some(lsubrs) -> lsubrs
                in
                inj_dec @@ Decode.Cff.lexical_charstring cff ~gsubrs ~lsubrs gid >>= function
                | None ->
                    err @@ GlyphNotFound(gid)

                | Some(gsubrs, lsubrs, lcs) ->
                    let lsubrs_map = lsubrs_map |> LsiMap.add fdindex lsubrs in
                    return (Alist.extend lcsacc (lcs, Some(fdindex), name), gsubrs, FDLsubrs(lsubrs_map))
          end
    ) gids (Alist.empty, Lsi.empty, lsubrs_info) >>= fun (charstring_acc_old, gsubrs_old, lsubrs_info_old) ->
  let (num_subrs, renumber_map, subrs_old) =
    let acc =
      Lsi.fold (fun i_old lcs_old (i_new, renumber_map, subr_old_acc) ->
        let subr_old_acc = Alist.extend subr_old_acc lcs_old in
        let renumber_map = renumber_map |> RenumberMap.add (Old.Global(i_old)) i_new in
        (i_new + 1, renumber_map, subr_old_acc)
      ) gsubrs_old (0, RenumberMap.empty, Alist.empty)
    in
    let (i_new, renumber_map, subr_old_acc) =
      match lsubrs_info_old with
      | SingleLsubrs(lsubrs) ->
          Lsi.fold (fun i_old lcs_old (i_new, renumber_map, subr_old_acc) ->
            let subr_old_acc = Alist.extend subr_old_acc lcs_old in
            let renumber_map = renumber_map |> RenumberMap.add (Old.Local(None, i_old)) i_new in
            (i_new + 1, renumber_map, subr_old_acc)
          ) lsubrs acc

      | FDLsubrs(lsubrs_map) ->
          LsiMap.fold (fun fdindex lsubrs acc ->
            Lsi.fold (fun i_old lcs_old (i_new, renumber_map, subr_old_acc) ->
              let subr_old_acc = Alist.extend subr_old_acc lcs_old in
              let renumber_map = renumber_map |> RenumberMap.add (Old.Local(Some(fdindex), i_old)) i_new in
              (i_new + 1, renumber_map, subr_old_acc)
            ) lsubrs acc
          ) lsubrs_map acc
    in
    (i_new, renumber_map, subr_old_acc |> Alist.to_list)
  in
  let bias =
    if num_subrs < 1240 then
      107
    else if num_subrs < 33900 then
      1131
    else
      32768
  in
  subrs_old |> mapM (renumber_subroutine ~fdindex_opt:None ~bias renumber_map) >>= fun gsubrs ->
    (* Assume here that no global subroutines are dependent on local subroutines. *)
  let charstrings_old = Alist.to_list charstring_acc_old in
  charstrings_old |> mapM (fun (lcs, fdindex_opt, name) ->
    renumber_subroutine ~fdindex_opt ~bias renumber_map lcs >>= fun charstring ->
    return (name, charstring)
  ) >>= fun names_and_charstrings ->
  inj_enc @@ Encode.Cff.make { top_dict with number_of_glyphs = num_glyphs } ~gsubrs ~names_and_charstrings


let make_cff_subset ~(omit_cmap : bool) (cff : Decode.cff_source) (gids : glyph_id list) : string ok =
  let open ResultMonad in

  let src = Decode.Cff(cff) in
  let num_glyphs = List.length gids in

  let maxp = Intermediate.Cff.Maxp.{ num_glyphs = num_glyphs } in
  inj_enc @@ Encode.Cff.Maxp.make maxp >>= fun table_maxp ->

  make_cff ~num_glyphs cff gids >>= fun table_cff ->

  make_cff_hmtx cff gids >>= fun (hmtx_result, bbox_all) ->

  make_common
    ~omit_cmap
    ~bbox_all
    ~index_to_loc_format:Intermediate.ShortLocFormat
    ~hmtx_result
    src gids >>= fun common_tables ->

  inj_enc @@ Encode.make_font_data_from_tables ~ttf:false @@
    List.append common_tables [
      table_maxp;
      table_cff;
    ]


let make ?(omit_cmap = false) (src : Decode.source) (gids : glyph_id list) : string ok =
  match src with
  | Ttf(ttf) -> make_ttf_subset ~omit_cmap ttf gids
  | Cff(cff) -> make_cff_subset ~omit_cmap cff gids
