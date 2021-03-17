
open Basic
open Value
open DecodeBasic
open DecodeOperation.Open


let d_loca_short (gid : glyph_id) : ((int * int) option) decoder =
  let open DecodeOperation in
  (* The position is set to the begging of `loca` table. *)
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
  (* The position is set to the begging of `loca` table. *)
  d_skip (gid * 4) >>= fun () ->
  d_uint32_int >>= fun glyf_offset1 ->
  d_uint32_int >>= fun glyf_offset2 ->
  if glyf_offset1 = glyf_offset2 then
    return None
  else
    return @@ Some(glyf_offset1, glyf_offset2 - glyf_offset1)


let d_loca (loc_format : loc_format) (gid : glyph_id) : (ttf_glyph_location option) decoder =
  let open DecodeOperation in
  (* The position is set to the begging of `loca` table. *)
  let dec =
    match loc_format with
    | ShortLocFormat -> d_loca_short gid
    | LongLocFormat  -> d_loca_long gid
  in
  dec >>= function
  | None                         -> return None
  | Some((glyf_offset, _length)) -> return @@ Some(TtfGlyphLocation(glyf_offset))


let loca (ttf : ttf_source) (gid : glyph_id) : (ttf_glyph_location option) ok =
  let open ResultMonad in
  let common = ttf.ttf_common in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_loca >>= fun (offset, _length) ->
  DecodeOperation.run common.core offset (d_loca common.loc_format gid)


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


type flag = {
  on_curve       : bool;
  x_short_vector : bool;
  y_short_vector : bool;
  this_x_is_same : bool;
  this_y_is_same : bool;
}


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
        {
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


let combine (endPtsOfContours : int list) (num_points : int) (flags : flag list) (xCoordinates : int list) (yCoordinates : int list) =
  let rec aux pointacc contouracc endPtsOfContours = function
    | (i, [], [], []) ->
        assert (i = num_points);
        assert (Alist.is_empty pointacc);
        Alist.to_list contouracc

    | (
        i,
        flag        :: flags,
        xCoordinate :: xCoordinates,
        yCoordinate :: yCoordinates
      ) ->
        let point = (flag.on_curve, xCoordinate, yCoordinate) in
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
        assert false
  in
  aux Alist.empty Alist.empty endPtsOfContours (0, flags, xCoordinates, yCoordinates)


let d_simple_glyph (numberOfContours : int) : ttf_simple_glyph_description decoder =
  let open DecodeOperation in
  if numberOfContours = 0 then
    return []
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
    d_skip instructionLength >>= fun () ->
    d_flags num_points >>= fun flagacc ->
    let flags = Alist.to_list flagacc in
    d_x_coordinates flags >>= fun xCoordinates ->
    d_y_coordinates flags >>= fun yCoordinates ->
    return (combine endPtsOfContours num_points flags xCoordinates yCoordinates)


type component_flag = {
  arg_1_and_2_are_words    : bool;
  args_are_xy_values       : bool;
  round_xy_to_grid         : bool;
  we_have_a_scale          : bool;
  we_have_an_x_and_y_scale : bool;
  we_have_a_two_by_two     : bool;
  we_have_instructions     : bool;
  use_my_metrics           : bool;
}


let d_component_flag : (bool * component_flag) decoder =
  let open DecodeOperation in
  d_uint16 >>= fun twobytes ->
  let more_components = (twobytes land 32 > 0) in
  let cflag =
    {
      arg_1_and_2_are_words    = (twobytes land 1 > 0);
      args_are_xy_values       = (twobytes land 2 > 0);
      round_xy_to_grid         = (twobytes land 4 > 0);
      we_have_a_scale          = (twobytes land 8 > 0);
      we_have_an_x_and_y_scale = (twobytes land 64 > 0);
      we_have_a_two_by_two     = (twobytes land 128 > 0);
      we_have_instructions     = (twobytes land 256 > 0);
      use_my_metrics           = (twobytes land 512 > 0);
    }
  in
  return (more_components, cflag)


let d_composite_glyph : ttf_composite_glyph_description decoder =
  let open DecodeOperation in
  let rec aux acc =
    d_component_flag >>= fun (more_components, cflags) ->
    d_uint16 >>= fun glyphIndex ->
    let dec = if cflags.arg_1_and_2_are_words then d_int16 else d_int8 in
    dec >>= fun argument1 ->
    dec >>= fun argument2 ->
    begin
      if cflags.we_have_a_scale then
        d_f2dot14 >>= fun scale ->
        return @@ Some{a = scale; b = 0.; c = 0.; d = scale}
      else if cflags.we_have_an_x_and_y_scale then
        d_f2dot14 >>= fun xscale ->
        d_f2dot14 >>= fun yscale ->
        return @@ Some{a = xscale; b = 0.; c = 0.; d = yscale}
      else if cflags.we_have_a_two_by_two then
        d_f2dot14 >>= fun a ->
        d_f2dot14 >>= fun b ->
        d_f2dot14 >>= fun c ->
        d_f2dot14 >>= fun d ->
        return @@ Some{a = a; b = b; c = c; d = d}
      else
        return None
    end >>= fun linear_transform ->
    let v =
      if cflags.args_are_xy_values then
        Vector(argument1, argument2)
      else
        Matching(argument1, argument2)
    in
    let acc = Alist.extend acc (glyphIndex, v, linear_transform) in
    if more_components then
      aux acc
    else
      return @@ Alist.to_list acc
  in
  aux Alist.empty


let d_glyf =
  let open DecodeOperation in
  (* The position is set to the beginning of a glyph. See 5.3.3.1 *)
  d_int16 >>= fun numberOfContours ->
  d_int16 >>= fun xMin ->
  d_int16 >>= fun yMin ->
  d_int16 >>= fun xMax ->
  d_int16 >>= fun yMax ->
  let bbox = { x_min = xMin; y_min = yMin; x_max = xMax; y_max = yMax } in
  if numberOfContours < -1 then
    err @@ Error.InvalidCompositeFormat(numberOfContours)
  else if numberOfContours = -1 then
    d_composite_glyph >>= fun components ->
    return (TtfCompositeGlyph(components), bbox)
  else
    d_simple_glyph numberOfContours >>= fun contours ->
    return (TtfSimpleGlyph(contours), bbox)


let glyf (ttf : ttf_source) (TtfGlyphLocation(reloffset) : ttf_glyph_location) : (ttf_glyph_description * bounding_box) ok =
  let open ResultMonad in
  let common = ttf.ttf_common in
  DecodeOperation.seek_required_table common.table_directory Value.Tag.table_glyf >>= fun (offset, _length) ->
  d_glyf |> DecodeOperation.run common.core (offset + reloffset)


let path_of_ttf_contour (contour : ttf_contour) : quadratic_path ok =
  let open ResultMonad in
  begin
    match contour with
    | (_, x0, y0) :: tail -> return ((x0, y0), tail)
    | []                  -> err Error.InvalidTtfContour
  end >>= fun (pt0, tail) ->
  let rec aux acc = function
    | []                                                  -> Alist.to_list acc
    | (true, x, y) :: tail                                -> aux (Alist.extend acc @@ QuadraticLineTo(x, y)) tail
    | (false, x1, y1) :: (true, x, y) :: tail             -> aux (Alist.extend acc @@ QuadraticCurveTo((x1, y1), (x, y))) tail
    | (false, x1, y1) :: (((false, x2, y2) :: _) as tail) -> aux (Alist.extend acc @@ QuadraticCurveTo((x1, y1), ((x1 + x2) / 2, (y1 + y2) / 2))) tail
    | (false, x1, y1) :: []                               -> Alist.to_list (Alist.extend acc @@ QuadraticCurveTo((x1, y1), pt0))
  in
  let elems = aux Alist.empty tail in
  return @@ (pt0, elems)
