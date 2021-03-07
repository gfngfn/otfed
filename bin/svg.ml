
module Alist = Otfed.Alist
module ResultMonad = Otfed.ResultMonad
module D = Otfed.Decode
module V = Otfed.Value


let dpoffset = 50

let display_x_scheme _units_per_em x = x
let display_y_scheme units_per_em y = units_per_em - y

let circle_scheme units_per_em ~color x y =
  Printf.sprintf "<circle cx=\"%d\" cy=\"%d\" r=\"5\" fill=\"%s\" />"
    (display_x_scheme units_per_em x)
    (display_y_scheme units_per_em y)
    color


let path_string_of_quadratic units_per_em (qpath : V.quadratic_path) =
  let display_x = display_x_scheme units_per_em in
  let display_y = display_y_scheme units_per_em in
  let circle = circle_scheme units_per_em in
  let ((x0, y0), qelems) = qpath in
  let curve0 = Printf.sprintf "M%d,%d" (display_x x0) (display_y y0) in
  let circ0 = circle ~color:"green" x0 y0 in
  let (curveacc, circacc) =
    qelems |> List.fold_left (fun (curveacc, circacc) qelem ->
      match qelem with
      | V.QuadraticLineTo((xto, yto)) ->
          let circ = circle ~color:"green" xto yto in
          let curve = Printf.sprintf "L%d,%d" (display_x xto) (display_y yto) in
          (Alist.extend curveacc curve, Alist.extend circacc circ)

      | V.QuadraticCurveTo((x1, y1), (xto, yto)) ->
          let circ1 = circle ~color:"orange" x1 y1 in
          let circto = circle ~color:"green" xto yto in
          let curve = Printf.sprintf "Q%d,%d %d,%d" (display_x x1) (display_y y1) (display_x xto) (display_y yto) in
          (Alist.extend curveacc curve, Alist.append circacc [circ1; circto])
    ) (Alist.extend Alist.empty curve0, Alist.extend Alist.empty circ0)
  in
  let curves = Alist.to_list curveacc in
  let circs = Alist.to_list circacc in
  (Printf.sprintf "<path d=\"%s Z\" fill=\"none\" stroke=\"red\" stroke-width=\"5\" />" (String.concat " " curves), String.concat "" circs)


let path_string_of_cubic units_per_em (cpath : V.cubic_path) =
  let display_x = display_x_scheme units_per_em in
  let display_y = display_y_scheme units_per_em in
  let circle = circle_scheme units_per_em in
  let ((x0, y0), celems) = cpath in
  let curve0 = Printf.sprintf "M%d,%d" (display_x x0) (display_y y0) in
  let circ0 = circle ~color:"green" x0 y0 in
  let (curveacc, circacc) =
    celems |> List.fold_left (fun (curveacc, circacc) celem ->
      match celem with
      | V.CubicLineTo((xto, yto)) ->
          let circ = circle ~color:"green" xto yto in
          let curve = Printf.sprintf "L%d,%d" (display_x xto) (display_y yto) in
          (Alist.extend curveacc curve, Alist.extend circacc circ)

      | V.CubicCurveTo((x1, y1), (x2, y2), (xto, yto)) ->
          let circ1 = circle ~color:"orange" x1 y1 in
          let circ2 = circle ~color:"orange" x2 y2 in
          let circto = circle ~color:"green" xto yto in
          let curve =
            Printf.sprintf "C%d,%d %d,%d %d,%d"
              (display_x x1)
              (display_y y1)
              (display_x x2)
              (display_y y2)
              (display_x xto)
              (display_y yto)
          in
          (Alist.extend curveacc curve, Alist.append circacc [circ1; circ2; circto])
    ) (Alist.extend Alist.empty curve0, Alist.extend Alist.empty circ0)
  in
  let curves = Alist.to_list curveacc in
  let circs = Alist.to_list circacc in
  (Printf.sprintf "<path d=\"%s Z\" fill=\"none\" stroke=\"red\" stroke-width=\"5\" />" (String.concat " " curves), String.concat "" circs)


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


let frame_em ~units_per_em ~y_max ~y_min =
  [
    Printf.sprintf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"purple\" stroke-width=\"5\" />"
      (display_x_scheme units_per_em 0)
      (display_y_scheme units_per_em y_max)
      units_per_em
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


let make_ttf_simple (ttfcontours : V.ttf_simple_glyph_description) (bbox : V.bounding_box) (units_per_em : int) =
  let open ResultMonad in
  let display_x = display_x_scheme units_per_em in
  let display_y = display_y_scheme units_per_em in
  ttfcontours |> mapM D.path_of_ttf_contour >>= fun qpaths ->
  let y_min = bbox.V.y_min in
  let y_max = bbox.V.y_max in

  let pcs = (qpaths |> List.map (path_string_of_quadratic units_per_em)) in
  let paths = List.map (fun (x, _) -> x) pcs in
  let circs = List.map (fun (_, y) -> y) pcs in
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
    let frame1 = frame_em ~units_per_em ~y_min ~y_max in
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


let make_ttf (descr : V.ttf_glyph_description) ~bbox:(bbox : V.bounding_box) ~units_per_em:(units_per_em : int) =
  let open ResultMonad in
  match descr with
  | V.TtfSimpleGlyph(simple) -> make_ttf_simple simple bbox units_per_em
  | V.TtfCompositeGlyph(_)   -> return ""


let make_cff (cpaths : V.cubic_path list) ~units_per_em:(units_per_em : int) =
  let display_x = display_x_scheme units_per_em in
  let display_y = display_y_scheme units_per_em in
  let pcs = cpaths |> List.map (path_string_of_cubic units_per_em) in
  let paths = List.map (fun (x, _) -> x) pcs in
  let circs = List.map (fun (_, y) -> y) pcs in
  let y_min = 0 in  (* temporary; should use bbox *)
  let y_max = units_per_em in  (* temporary; should use bbox *)
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
    let frame1 = frame_em ~units_per_em ~y_min ~y_max in
    let frame2 = frame_bbox ~units_per_em ~bbox:{ x_min = 0; y_min; x_max = units_per_em; y_max } in
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
