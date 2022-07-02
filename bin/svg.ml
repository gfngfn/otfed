
module Alist = Otfed.Alist
module ResultMonad = Otfed.ResultMonad
module D = Otfed.Decode
module V = Otfed.Value


let dpoffset = 50

let circle_radius = 5
let index_x_offset = 5
let index_y_offset = -5

let display_x_scheme _units_per_em x = x
let display_y_scheme units_per_em y = units_per_em - y


let circle_scheme ~units_per_em ~index:index_opt ~color x y =
  let dispx = display_x_scheme units_per_em x in
  let dispy = display_y_scheme units_per_em y in
  let s =
    Printf.sprintf "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" fill=\"%s\" />"
      dispx dispy circle_radius color
  in
  match index_opt with
  | None ->
      s

  | Some(i) ->
      Printf.sprintf "%s<text x=\"%d\" y=\"%d\">%d</text>"
        s (dispx + index_x_offset) (dispy + index_y_offset) i


let path_string_of_quadratic ~units_per_em ~shift:(vx, vy) ~index:(i : int) (qpath : V.quadratic_path) =
  let display_x x = display_x_scheme units_per_em (x + vx) in
  let display_y y = display_y_scheme units_per_em (y + vy) in
  let circle ~color ~index x y = circle_scheme ~units_per_em ~index ~color (x + vx) (y + vy) in
  let ((x0, y0), qelems) = qpath in
  let curve0 = Printf.sprintf "M%d,%d" (display_x x0) (display_y y0) in
  let circ0 = circle ~color:"green" ~index:(Some(i)) x0 y0 in
  let (i, curveacc, circacc) =
    qelems |> List.fold_left (fun (i, curveacc, circacc) qelem ->
      match qelem with
      | V.QuadraticLineTo((xto, yto)) ->
          let circ = circle ~color:"green" ~index:(Some(i)) xto yto in
          let curve = Printf.sprintf "L%d,%d" (display_x xto) (display_y yto) in
          (i + 1, Alist.extend curveacc curve, Alist.extend circacc circ)

      | V.QuadraticCurveTo((x1, y1), (xto, yto)) ->
          let circ1 = circle ~color:"orange" ~index:None x1 y1 in
          let circto = circle ~color:"green" ~index:(Some(i)) xto yto in
          let curve = Printf.sprintf "Q%d,%d %d,%d" (display_x x1) (display_y y1) (display_x xto) (display_y yto) in
          (i + 1, Alist.extend curveacc curve, Alist.append circacc [circ1; circto])
    ) (i + 1, Alist.extend Alist.empty curve0, Alist.extend Alist.empty circ0)
  in
  let curves = Alist.to_list curveacc in
  let circs = Alist.to_list circacc in
  (i, Printf.sprintf "<path d=\"%s Z\" fill=\"none\" stroke=\"red\" stroke-width=\"5\" />" (String.concat " " curves), String.concat "" circs)


let path_string_of_cubic ~units_per_em ~index:(i : int) (cpath : V.cubic_path) =
  let display_x = display_x_scheme units_per_em in
  let display_y = display_y_scheme units_per_em in
  let circle = circle_scheme ~units_per_em in
  let ((x0, y0), celems) = cpath in
  let curve0 = Printf.sprintf "M%d,%d" (display_x x0) (display_y y0) in
  let circ0 = circle ~color:"green" ~index:(Some(i)) x0 y0 in
  let (i, curveacc, circacc) =
    celems |> List.fold_left (fun (i, curveacc, circacc) celem ->
      match celem with
      | V.CubicLineTo((xto, yto)) ->
          let circ = circle ~color:"green" ~index:(Some(i)) xto yto in
          let curve = Printf.sprintf "L%d,%d" (display_x xto) (display_y yto) in
          (i + 1, Alist.extend curveacc curve, Alist.extend circacc circ)

      | V.CubicCurveTo((x1, y1), (x2, y2), (xto, yto)) ->
          let circ1 = circle ~color:"orange" ~index:None x1 y1 in
          let circ2 = circle ~color:"orange" ~index:None x2 y2 in
          let circto = circle ~color:"green" ~index:(Some(i)) xto yto in
          let curve =
            Printf.sprintf "C%d,%d %d,%d %d,%d"
              (display_x x1)
              (display_y y1)
              (display_x x2)
              (display_y y2)
              (display_x xto)
              (display_y yto)
          in
          (i + 1, Alist.extend curveacc curve, Alist.append circacc [circ1; circ2; circto])
    ) (i + 1, Alist.extend Alist.empty curve0, Alist.extend Alist.empty circ0)
  in
  let curves = Alist.to_list curveacc in
  let circs = Alist.to_list circacc in
  (i, Printf.sprintf "<path d=\"%s Z\" fill=\"none\" stroke=\"red\" stroke-width=\"5\" />" (String.concat " " curves), String.concat "" circs)


let svg_prefix_and_suffix ~height ~width ~view_box:(x_min, y_min, x_max, y_max) =
  let prefix =
    [
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>";
      "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">";
      Printf.sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"%d\" height=\"%d\" viewBox=\"%d %d %d %d\">"
        height width x_min y_min x_max y_max;
    ]
  in
  let suffix = ["</svg>"] in
  (prefix, suffix)


let frame_aw ~units_per_em ~y_max ~y_min ~aw =
  [
    Printf.sprintf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"purple\" stroke-width=\"5\" />"
      (display_x_scheme units_per_em 0)
      (display_y_scheme units_per_em y_max)
      aw
      (y_max - y_min);
  ]


let frame_bbox ~units_per_em ~bbox =
  let x_min = bbox.V.x_min in
  let y_min = bbox.V.y_min in
  let x_max = bbox.V.x_max in
  let y_max = bbox.V.y_max in
  [
    Printf.sprintf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"blue\" stroke-width=\"5\" />"
      (display_x_scheme units_per_em x_min)
      (display_y_scheme units_per_em y_max)
      (x_max - x_min)
      (y_max - y_min);
  ]


let make_ttf_simple ~shift ((contours, _) : V.Ttf.simple_glyph_description) (units_per_em : int) =
  let open ResultMonad in
  contours |> mapM D.Ttf.path_of_contour >>= fun qpaths ->

  let (_, pathacc, circacc) =
    qpaths |> List.fold_left (fun (i, pathacc, circacc) qpath ->
      let (i, path, circ) = path_string_of_quadratic ~units_per_em ~shift ~index:i qpath in
      (i, Alist.extend pathacc path, Alist.extend circacc circ)
    ) (0, Alist.empty, Alist.empty)
  in
  let paths = Alist.to_list pathacc in
  let circs = Alist.to_list circacc in
  return (paths, circs)


let make_ttf (simples : (V.Ttf.simple_glyph_description * (V.design_units * V.design_units)) list) ~bbox:(bbox : V.bounding_box) ~units_per_em:(units_per_em : int) ~aw:(aw : int) =
  let open ResultMonad in
  foldM (fun (pathacc, circacc) (simple, v) ->
    make_ttf_simple ~shift:v simple units_per_em >>= fun (paths, circs) ->
    return (Alist.append pathacc paths, Alist.append circacc circs)
  ) simples (Alist.empty, Alist.empty) >>= fun (pathacc, circacc) ->
  let paths = Alist.to_list pathacc in
  let circs = Alist.to_list circacc in
  let ss =
    let display_x = display_x_scheme units_per_em in
    let display_y = display_y_scheme units_per_em in
    let y_min = bbox.V.y_min in
    let y_max = bbox.V.y_max in
    let (prefix, suffix) =
      svg_prefix_and_suffix
        ~height:(units_per_em + 2 * dpoffset)
        ~width:(y_max - y_min + 2 * dpoffset)
        ~view_box:(
          display_x (0 - dpoffset),
          display_y (y_max + dpoffset),
          units_per_em + 2 * dpoffset,
          y_max - y_min + 2 * dpoffset
        )
    in
    let frame1 = frame_aw ~units_per_em ~y_min ~y_max ~aw in
    let frame2 = frame_bbox ~units_per_em ~bbox in
    List.concat [
      prefix;
      frame1;
      frame2;
      paths;
      circs;
      suffix;
    ]
  in
  return @@ String.concat "" ss


let make_cff (cpaths : V.cubic_path list) ~units_per_em:(units_per_em : int) ~aw:(aw : int) =
  let display_x = display_x_scheme units_per_em in
  let display_y = display_y_scheme units_per_em in
  let (_, pathacc, circacc) =
    cpaths |> List.fold_left (fun (i, pathacc, circacc) cpath ->
      let (i, path, circ) = path_string_of_cubic ~units_per_em ~index:i cpath in
      (i, Alist.extend pathacc path, Alist.extend circacc circ)
    ) (0, Alist.empty, Alist.empty)
  in
  let paths = Alist.to_list pathacc in
  let circs = Alist.to_list circacc in
  let bbox =
    match V.calculate_bounding_box_of_paths cpaths with
    | None       -> V.{ x_min = 0; y_min = 0; x_max = aw; y_max = units_per_em }
    | Some(bbox) -> bbox
  in
  let y_min = bbox.V.y_min in
  let y_max = bbox.V.y_max in
  let ss =
    let (prefix, suffix) =
      svg_prefix_and_suffix
        ~height:(units_per_em + 2 * dpoffset)
        ~width:(y_max - y_min + 2 * dpoffset)
        ~view_box:(
          display_x (0 - dpoffset),
          display_y (y_max + dpoffset),
          units_per_em + 2 * dpoffset,
          y_max - y_min + 2 * dpoffset
        )
    in
    let frame1 = frame_aw ~units_per_em ~y_min ~y_max ~aw in
    let frame2 = frame_bbox ~units_per_em ~bbox:bbox in
    List.concat [
      prefix;
      frame1;
      frame2;
      paths;
      circs;
      suffix;
    ]
  in
  String.concat "" ss
