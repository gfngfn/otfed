
open Basic
open Value
open EncodeBasic
open EncodeOperation.Open


module Maxp = EncodeTtfMaxp


let make_end_points (contours : Ttf.contour list) : int list =
  let (acc, _) =
    contours |> List.fold_left (fun (acc, i) contour ->
      let num_points_in_contour = List.length contour in
      let i_next = i + num_points_in_contour in
      (Alist.extend acc (i_next - 1), i_next)
    ) (Alist.empty, 0)
  in
  Alist.to_list acc


type flag = Intermediate.Ttf.flag

type relative =
  | Short of int (* non-negative *)
  | Long  of int

type coordinate_accumulator = {
  flags       : flag Alist.t;
  relative_xs : relative Alist.t;
  relative_ys : relative Alist.t;
}


let decompose_contours (contours : Ttf.contour list) : flag list * relative list * relative list =
  let coordinates = List.concat contours in
  let acc =
    {
      flags       = Alist.empty;
      relative_xs = Alist.empty;
      relative_ys = Alist.empty;
    }
  in
  let (acc, _, _) =
    coordinates |> List.fold_left (fun (acc, x_coord_prev, y_coord_prev) elem ->
      let Ttf.{ on_curve; point = (x_coord, y_coord) } = elem in
      let (x_short_vector, this_x_is_same, relative_xs) =
        let x_rel = x_coord - x_coord_prev in
        if x_rel = 0 then
          (false, true, acc.relative_xs)
        else if -256 < x_rel && x_rel < 256 then
          (* Note that the valid interval is not [-256, 255], but [-255, 255].
             This is because we possibly encode the absolute value of `y_rel` as 8-bit unsigned integer. *)
          let (is_positive, x_rel_abs) =
            if x_rel > 0 then (true, x_rel) else (false, -x_rel)
          in
          (true, is_positive, Alist.extend acc.relative_xs (Short(x_rel_abs)))
        else
          (false, false, Alist.extend acc.relative_xs (Long(x_rel)))
      in
      let (y_short_vector, this_y_is_same, relative_ys) =
        let y_rel = y_coord - y_coord_prev in
        if y_rel = 0 then
          (false, true, acc.relative_ys)
        else if -256 < y_rel && y_rel < 256 then
          (* Note that the valid interval is not [-256, 255], but [-255, 255].
             This is because we possibly encode the absolute value of `y_rel` as 8-bit unsigned integer. *)
          let (is_positive, y_rel_abs) =
            if y_rel > 0 then (true, y_rel) else (false, -y_rel)
          in
          (true, is_positive, Alist.extend acc.relative_ys (Short(y_rel_abs)))
        else
          (false, false, Alist.extend acc.relative_ys (Long(y_rel)))
      in
      let flag =
        Intermediate.Ttf.{
          on_curve;
          this_x_is_same;
          this_y_is_same;
          x_short_vector;
          y_short_vector;
        }
      in
      let acc =
        {
          flags = Alist.extend acc.flags flag;
          relative_xs;
          relative_ys;
        }
      in
      (acc, x_coord, y_coord)
    ) (acc, 0, 0)
  in
  (Alist.to_list acc.flags, Alist.to_list acc.relative_xs, Alist.to_list acc.relative_ys)


let e_flag (flag : flag) =
  let open EncodeOperation in
  let open Intermediate.Ttf in
  e_8bits [
    flag.on_curve;
    flag.x_short_vector;
    flag.y_short_vector;
    false; (* repeat *)
    flag.this_x_is_same;
    flag.this_y_is_same;
  ]


let e_relative (relative : relative) =
  let open EncodeOperation in
  match relative with
  | Short(rel_abs) -> e_uint8 rel_abs
  | Long(rel)      -> e_int16 rel


let e_simple_glyph ((contours, instructions) : Ttf.simple_glyph_description) : unit encoder =
  let endPtsOfContours = make_end_points contours in
  let instructionLength = String.length instructions in
  let (flags, xCoordinates, yCoordinates) = decompose_contours contours in
  let open EncodeOperation in
  e_list e_uint16 endPtsOfContours >>= fun () ->
  e_uint16 instructionLength       >>= fun () ->
  e_bytes instructions             >>= fun () ->
  e_list e_flag flags              >>= fun () ->
  e_list e_relative xCoordinates   >>= fun () ->
  e_list e_relative yCoordinates   >>= fun () ->
  return ()


let e_component_flag (more_components : bool) (cflag : Intermediate.Ttf.component_flag) =
  let open EncodeOperation in
  let open Intermediate.Ttf in
  e_16bits [
    cflag.arg_1_and_2_are_words;
    cflag.args_are_xy_values;
    cflag.round_xy_to_grid;
    cflag.we_have_a_scale;
    false; (* a reserved bit *)
    more_components;
    cflag.we_have_an_x_and_y_scale;
    cflag.we_have_a_two_by_two;
    cflag.we_have_instructions;
    cflag.use_my_metrics;
    false; (* a reserved bit *)
    false; (* a reserved bit *)
    cflag.unscaled_component_offset;
  ]


let e_composite_glyph (composite_glyph : Ttf.composite_glyph_description) : unit encoder =
  let open EncodeOperation in
  let rec aux = function
    | [] ->
        begin
          match composite_glyph.Value.Ttf.composite_instruction with
          | None    -> return ()
          | Some(s) -> e_bytes s
        end

    | component :: elems ->
        let gid = component.Value.Ttf.component_glyph_id in
        let composition = component.Value.Ttf.composition in
        let linear_opt = component.Value.Ttf.component_scale in
        let (args_are_xy_values, arg1, arg2) =
          match composition with
          | Ttf.Vector(x, y)   -> (true, x, y)
          | Ttf.Matching(i, j) -> (false, i, j)
        in
        let (arg_1_and_2_are_words, e_arg) =
          if -128 <= arg1 && arg1 < 128 && -128 <= arg2 && arg2 < 128 then
            (false, e_int8)
          else
            (true, e_int16)
        in
        let we_have_instructions =
          match (elems, composite_glyph.Value.Ttf.composite_instruction) with
          | ([], Some(_)) -> true
          | _             -> false
        in
        let cflags_base =
          Intermediate.Ttf.{
            arg_1_and_2_are_words;
            args_are_xy_values;
            round_xy_to_grid          = component.Value.Ttf.round_xy_to_grid;
            we_have_a_scale           = false;
            we_have_an_x_and_y_scale  = false;
            we_have_a_two_by_two      = false;
            we_have_instructions;
            use_my_metrics            = component.Value.Ttf.use_my_metrics;
            unscaled_component_offset = component.Value.Ttf.unscaled_component_offset;
          }
        in
        let (cflags, e_linear_transform) =
          match linear_opt with
          | Some{a = a; b = b; c = c; d = d} ->
              if b = 0. && c = 0. then
                if a = d then
                  let enc =
                    e_f2dot14 a
                  in
                  ({ cflags_base with we_have_a_scale = true }, enc)
                else
                  let enc =
                    e_f2dot14 a >>= fun () ->
                    e_f2dot14 d
                  in
                  ({ cflags_base with we_have_an_x_and_y_scale = true }, enc)
              else
                let enc =
                    e_f2dot14 a >>= fun () ->
                    e_f2dot14 b >>= fun () ->
                    e_f2dot14 c >>= fun () ->
                    e_f2dot14 d
                in
                ({ cflags_base with we_have_a_two_by_two = true }, enc)

          | None ->
              (cflags_base, return ())
        in
        let more_components =
          match elems with
          | []     -> false
          | _ :: _ -> true
        in
        e_component_flag more_components cflags >>= fun () ->
        e_uint16 gid >>= fun () ->
        e_arg arg1 >>= fun () ->
        e_arg arg2 >>= fun () ->
        e_linear_transform >>= fun () ->
        aux elems
  in
  aux composite_glyph.Value.Ttf.composite_components


let e_glyph (g : Ttf.glyph_info) =
  let open EncodeOperation in
  let (number_of_contours, enc) =
    match g.description with
    | Ttf.SimpleGlyph(simple_glyph) ->
        let (contours, _) = simple_glyph in
        (List.length contours, e_simple_glyph simple_glyph)

    | Ttf.CompositeGlyph(elems) ->
        (-1, e_composite_glyph elems)
  in
  e_int16 number_of_contours   >>= fun () ->
  e_int16 g.bounding_box.x_min >>= fun () ->
  e_int16 g.bounding_box.y_min >>= fun () ->
  e_int16 g.bounding_box.x_max >>= fun () ->
  e_int16 g.bounding_box.y_max >>= fun () ->
  enc


type glyph_relative_offset =
  | GlyphRelativeOffset of int


let make_glyf (gs : (Ttf.glyph_info option) list) : (table * glyph_relative_offset list) ok =
  let enc =
    let open EncodeOperation in
    gs |> mapM (function
    | None ->
      (* Encoding an empty glyph *)
        current             >>= fun reloffset ->
        return @@ GlyphRelativeOffset(reloffset)

    | Some(g) ->
        e_glyph g           >>= fun () ->
        pad_to_long_aligned >>= fun () ->
          (* Every glyph location should begin and end at long-aligned local offsets. *)
        current             >>= fun reloffset ->
        return @@ GlyphRelativeOffset(reloffset)
    )
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, locs) ->
  let table =
    {
      tag = Value.Tag.table_glyf;
      contents;
    }
  in
  return (table, locs)


let can_use_short_loc_format (locs : glyph_relative_offset list) : bool =
  match List.rev locs with
  | []                                       -> true
  | GlyphRelativeOffset(last_reloffset) :: _ -> last_reloffset / 2 < 65536


let make_loca (locs : glyph_relative_offset list) : (table * Intermediate.loc_format) ok =
  let enc =
    let open EncodeOperation in
    let open Intermediate in
    let (loc_format, e_single) =
      if can_use_short_loc_format locs then
        (ShortLocFormat, function GlyphRelativeOffset(reloffset) -> e_uint16 (reloffset / 2))
      else
        (LongLocFormat, function GlyphRelativeOffset(reloffset) -> e_uint32 (!% reloffset))
    in
    e_single (GlyphRelativeOffset(0)) >>= fun () ->
    e_list e_single locs              >>= fun () ->
    return loc_format
  in
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, loc_format) ->
  let table =
    {
      tag = Value.Tag.table_loca;
      contents;
    }
  in
  return (table, loc_format)
