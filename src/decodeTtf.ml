
open Basic
open Value
open DecodeBasic
open DecodeOperation.Open


module Maxp = DecodeTtfMaxp


let d_loca_short (gid : glyph_id) : ((int * int) option) decoder =
  let open DecodeOperation in
  (* The position is set to the beginning of a `loca` table. *)
  d_skip (gid * 2) >>= fun () ->
  d_uint16 >>= fun half1 ->
  d_uint16 >>= fun half2 ->
  if half1 = half2 then
    return None
  else
    let glyf_offset1 = half1 * 2 in
    let glyf_offset2 = half2 * 2 in
    return @@ Some(glyf_offset1, glyf_offset2 - glyf_offset1)


let d_loca_long (gid : glyph_id) : ((int * int) option) decoder =
  let open DecodeOperation in
  (* The position is set to the beginning of a `loca` table. *)
  d_skip (gid * 4) >>= fun () ->
  d_uint32_int >>= fun glyf_offset1 ->
  d_uint32_int >>= fun glyf_offset2 ->
  if glyf_offset1 = glyf_offset2 then
    return None
  else
    return @@ Some(glyf_offset1, glyf_offset2 - glyf_offset1)


let d_loca ~(num_glyphs : int) (loc_format : Intermediate.loc_format) (gid : glyph_id) : ((int * int) option) decoder =
  let open DecodeOperation in
  (* The position is set to the beginning of a `loca` table. *)
  if gid < 0 || num_glyphs <= gid then
    return None
  else
    match loc_format with
    | ShortLocFormat -> d_loca_short gid
    | LongLocFormat  -> d_loca_long gid


let loca (ttf : ttf_source) (gid : glyph_id) : (Intermediate.Ttf.glyph_location option) ok =
  let open ResultMonad in
  let common = ttf.ttf_common in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_loca >>= fun (offset, _length) ->
  let num_glyphs = common.num_glyphs in
  d_loca ~num_glyphs common.loc_format gid |> DecodeOperation.run common.core offset >>= function
  | None ->
      return None

  | Some((reloffset, length)) ->
      return @@ Some(Intermediate.Ttf.GlyphLocation{ reloffset; length })


let d_end_points (numberOfContours : int) : (int Alist.t) decoder =
  let open DecodeOperation in
  let rec loop i acc =
    if i <= 0 then
      return acc
    else
      d_uint16 >>= fun e ->
      loop (i - 1) (Alist.extend acc e)
  in
  loop numberOfContours Alist.empty


type flag = Intermediate.Ttf.flag


let d_flags (num_points : int) : (flag Alist.t) decoder =
  let rec extend_repeatedly acc n x =
    if n <= 0 then
      acc
    else
      extend_repeatedly (Alist.extend acc x) (n - 1) x
  in
  let rec aux i acc =
    let open DecodeOperation in
    if i <= 0 then
      return acc
    else
      d_uint8 >>= fun byte ->
      let flag =
        Intermediate.Ttf.{
          on_curve       = (byte land 1 > 0);
          x_short_vector = (byte land 2 > 0);
          y_short_vector = (byte land 4 > 0);
          this_x_is_same = (byte land 16 > 0);
          this_y_is_same = (byte land 32 > 0);
        }
      in
      let does_repeat = (byte land 8 > 0) in
      if does_repeat then
        d_uint8 >>= fun n ->
        aux (i - 1 - n) (extend_repeatedly acc (n + 1) flag)
      else
        aux (i - 1) (Alist.extend acc flag)
  in
  aux num_points Alist.empty


let d_coordinates is_short is_same (flags : flag list) : (int list) decoder =
  let open DecodeOperation in
  let rec aux x acc = function
    | [] ->
        return @@ Alist.to_list acc

    | flag :: flags ->
        begin
          if is_short flag then
            d_uint8 >>= fun dx ->
            return @@ x + (if is_same flag then dx else - dx)
          else
            if is_same flag then
              return x
            else
              d_int16 >>= fun dx ->
              return @@ x + dx
        end >>= fun x ->
        aux x (Alist.extend acc x) flags

  in
  aux 0 Alist.empty flags


let d_x_coordinates flags =
  d_coordinates (fun flag -> flag.x_short_vector) (fun flag -> flag.this_x_is_same) flags

let d_y_coordinates flags =
  d_coordinates (fun flag -> flag.y_short_vector) (fun flag -> flag.this_y_is_same) flags


let combine (endPtsOfContours : int list) (num_points : int) (flags : flag list) (xCoordinates : int list) (yCoordinates : int list) : (Ttf.contour list) decoder =
  let open DecodeOperation in
  let rec aux pointacc contouracc endPtsOfContours = function
    | (i, [], [], []) ->
        if i = num_points && Alist.is_empty pointacc then
          return @@ Alist.to_list contouracc
        else
          err @@ Error.InconsistentNumberOfPoints{
            num_points = num_points;
            num_flags  = List.length flags;
            num_xs     = List.length xCoordinates;
            num_ys     = List.length yCoordinates;
          }

    | (
        i,
        flag        :: flags,
        xCoordinate :: xCoordinates,
        yCoordinate :: yCoordinates
      ) ->
        let point =
          Ttf.{
            on_curve = flag.Intermediate.Ttf.on_curve;
            point    = (xCoordinate, yCoordinate);
          }
        in
        let (is_final, endPtsOfContours) =
          match endPtsOfContours with
          | []      -> (false, [])
          | e :: es -> if e = i then (true, es) else (false, endPtsOfContours)
        in
        let tuple = (i + 1, flags, xCoordinates, yCoordinates) in
        if is_final then
          let contour = Alist.to_list (Alist.extend pointacc point) in
          aux Alist.empty (Alist.extend contouracc contour) endPtsOfContours tuple
        else
          aux (Alist.extend pointacc point) contouracc endPtsOfContours tuple

    | _ ->
        err @@ Error.InconsistentNumberOfPoints{
          num_points = num_points;
          num_flags  = List.length flags;
          num_xs     = List.length xCoordinates;
          num_ys     = List.length yCoordinates;
        }
  in
  aux Alist.empty Alist.empty endPtsOfContours (0, flags, xCoordinates, yCoordinates)


let d_simple_glyph (numberOfContours : int) : Ttf.simple_glyph_description decoder =
  let open DecodeOperation in
  if numberOfContours = 0 then
    d_uint16 >>= fun instructionLength ->
    d_bytes instructionLength >>= fun instructions ->
    return ([], instructions)
  else
    d_end_points numberOfContours >>= fun endPtsOfContours ->
    (* `num_points`: the total number of points. *)
    let num_points =
      match Alist.chop_last endPtsOfContours with
      | None         -> assert false
      | Some((_, e)) -> e + 1
    in
    let endPtsOfContours = Alist.to_list endPtsOfContours in
    d_uint16 >>= fun instructionLength ->
    d_bytes instructionLength >>= fun instructions ->
    d_flags num_points >>= fun flagacc ->
    let flags = Alist.to_list flagacc in
    d_x_coordinates flags >>= fun xCoordinates ->
    d_y_coordinates flags >>= fun yCoordinates ->
    combine endPtsOfContours num_points flags xCoordinates yCoordinates >>= fun contours ->
    return (contours, instructions)


type component_flag = Intermediate.Ttf.component_flag


let d_component_flag : (bool * component_flag) decoder =
  let open DecodeOperation in
  d_uint16 >>= fun twobytes ->
  let more_components = (twobytes land 32 > 0) in
  let cflag =
    Intermediate.Ttf.{
      arg_1_and_2_are_words     = (twobytes land 1 > 0);
      args_are_xy_values        = (twobytes land 2 > 0);
      round_xy_to_grid          = (twobytes land 4 > 0);
      we_have_a_scale           = (twobytes land 8 > 0);
      we_have_an_x_and_y_scale  = (twobytes land 64 > 0);
      we_have_a_two_by_two      = (twobytes land 128 > 0);
      we_have_instructions      = (twobytes land 256 > 0);
      use_my_metrics            = (twobytes land 512 > 0);
      unscaled_component_offset = (twobytes land 4096 > 0);
    }
  in
  return (more_components, cflag)


let d_composite_glyph ~(start_offset : int) ~(length : int) : Ttf.composite_glyph_description decoder =
  let open DecodeOperation in
  let rec aux ~(start_offset : int) acc =
    d_component_flag >>= fun (more_components, cflags) ->
    d_uint16 >>= fun glyphIndex ->
    let dec = if cflags.arg_1_and_2_are_words then d_int16 else d_int8 in
    dec >>= fun argument1 ->
    dec >>= fun argument2 ->
    begin
      if cflags.we_have_a_scale then
        d_f2dot14 >>= fun scale ->
        return @@ Some({a = scale; b = 0.; c = 0.; d = scale})
      else if cflags.we_have_an_x_and_y_scale then
        d_f2dot14 >>= fun xscale ->
        d_f2dot14 >>= fun yscale ->
        return @@ Some({a = xscale; b = 0.; c = 0.; d = yscale})
      else if cflags.we_have_a_two_by_two then
        d_f2dot14 >>= fun a ->
        d_f2dot14 >>= fun b ->
        d_f2dot14 >>= fun c ->
        d_f2dot14 >>= fun d ->
        return @@ Some({a = a; b = b; c = c; d = d})
      else
        return None
    end >>= fun linear_transform ->
    let v =
      if cflags.args_are_xy_values then
        Ttf.Vector(argument1, argument2)
      else
        Ttf.Matching(argument1, argument2)
    in
    let component =
      Value.Ttf.{
        component_glyph_id        = glyphIndex;
        composition               = v;
        component_scale           = linear_transform;
        round_xy_to_grid          = cflags.round_xy_to_grid;
        use_my_metrics            = cflags.use_my_metrics;
        unscaled_component_offset = cflags.unscaled_component_offset;
      }
    in
    let acc = Alist.extend acc component in
    if more_components then
      aux ~start_offset acc
    else if cflags.we_have_instructions then
      current >>= fun end_offset ->
      d_bytes (length - (end_offset - start_offset)) >>= fun instructions ->
      return @@ Value.Ttf.{
        composite_components  = Alist.to_list acc;
        composite_instruction = Some(instructions);
      }
    else
      return @@ Value.Ttf.{
        composite_components  = Alist.to_list acc;
        composite_instruction = None;
      }
  in
  aux ~start_offset Alist.empty


let d_glyph ~(length : int) =
  let open DecodeOperation in
  (* The position is set to the beginning of a glyph. See 5.3.3.1 *)
  current >>= fun start_offset ->
  d_int16 >>= fun numberOfContours ->
  d_int16 >>= fun xMin ->
  d_int16 >>= fun yMin ->
  d_int16 >>= fun xMax ->
  d_int16 >>= fun yMax ->
  let bounding_box = { x_min = xMin; y_min = yMin; x_max = xMax; y_max = yMax } in
  if numberOfContours < -1 then
    err @@ Error.InvalidCompositeFormat(numberOfContours)
  else if numberOfContours = -1 then
    d_composite_glyph ~start_offset ~length >>= fun components ->
    return Ttf.{
      description = CompositeGlyph(components);
      bounding_box;
    }
  else
    d_simple_glyph numberOfContours >>= fun contours ->
    return Ttf.{
      description = SimpleGlyph(contours);
      bounding_box;
    }


let glyf (ttf : ttf_source) (gloc : Intermediate.Ttf.glyph_location) : Ttf.glyph_info ok =
  let open ResultMonad in
  let common = ttf.ttf_common in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_glyf >>= fun (offset, _length) ->
  let (Intermediate.Ttf.GlyphLocation{ reloffset; length }) = gloc in
  d_glyph ~length |> DecodeOperation.run common.core (offset + reloffset)


let path_of_contour (contour : Ttf.contour) : quadratic_path ok =
  let open ResultMonad in
  begin
    match contour with
    | []           -> err Error.InvalidTtfContour
    | elem :: tail -> return (elem.Ttf.point, tail)
  end >>= fun (pt0, tail) ->
  let rec aux acc = function
    | [] ->
        Alist.to_list acc

    | Ttf.{ on_curve = true; point = (x, y) } :: tail ->
        aux (Alist.extend acc @@ QuadraticLineTo(x, y)) tail

    | Ttf.{ on_curve = false; point = (x1, y1) } :: Ttf.{ on_curve = true; point = (x, y) } :: tail ->
        aux (Alist.extend acc @@ QuadraticCurveTo((x1, y1), (x, y))) tail

    | Ttf.{ on_curve = false; point = (x1, y1) } :: ((Ttf.{ on_curve = false; point = (x2, y2) } :: _) as tail) ->
        aux (Alist.extend acc @@ QuadraticCurveTo((x1, y1), ((x1 + x2) / 2, (y1 + y2) / 2))) tail

    | Ttf.{ on_curve = false; point = (x1, y1) } :: [] ->
        Alist.to_list (Alist.extend acc @@ QuadraticCurveTo((x1, y1), pt0))
  in
  let elems = aux Alist.empty tail in
  return @@ (pt0, elems)
